#!/usr/bin/env stack
-- stack script --resolver lts-13.17 --package shelly,text,exceptions --ghc-options -fno-warn-type-defaults

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Shelly
import Data.Text (Text)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM,forM_)
import Control.Monad.Catch (throwM,SomeException)

default (Text)

conc = T.concat

data RDSInstanceInfo = RDSInstanceInfo {
    pod :: Text
  , handle :: Text
  , username :: Text
  , password :: Text
} deriving Show

catchAskRetry :: Sh a -> Sh a
catchAskRetry action = flip handleany_sh action $ \exception -> do
    liftIO $ TIO.putStrLn "Exception caught. Retry? ([y]/n)"
    ans <- liftIO TIO.getLine
    case ans of
      "n" -> throwM exception
      _   -> catchAskRetry action

run' :: Text -> [Text] -> Sh Text
run' program args = do
    liftIO $ do
        TIO.putStr $ conc ["[*] Running `\x1b[32m",program]
        forM_ args $ \arg -> TIO.putStr $ conc [" ",arg]
        TIO.putStrLn "\x1b[0m`"
    run (fromText program) args

parseClusterNames :: Text -> Sh [Text]
parseClusterNames input = do
    let xs = tail $ T.splitOn "\n" input
    fmap init <$> forM xs $ \line -> do
        let line' = T.dropWhile (\c -> isSpace c || c == '*') line
        pure $ T.takeWhile (not . isSpace) line'

parsePods :: Text -> Sh [Text]
parsePods input = do
    let xs = T.splitOn "\n" input
    fmap init <$> forM xs $ \line -> pure $ T.takeWhile (not . isSpace) line

parseNonDefaultDatabases :: Text -> Sh [Text]
parseNonDefaultDatabases input = do
    let xs = tail $ tail $ T.splitOn "\n" input
    pure $ filter (\line -> line /= "information_schema"
                            && line /= "mysql"
                            && line /= "performance_schema"
                            && line /= "innodb"
                            && line /= T.empty ) xs

indexOptions :: [Text] -> [(Int,Text)]
indexOptions = zip [1..]

drawMenu :: [(Int,Text)] -> Sh ()
drawMenu indexedOptions = forM_ indexedOptions $ \ (i,option) -> echo $ conc [T.pack (show i),". ",option]

giveOption :: Text -> [Text] -> Sh Text
giveOption optionName options = catchAskRetry $ do
    let indexedOptions = indexOptions options
    drawMenu indexedOptions
    echo $ conc ["Select \x1b[31m",optionName,"\x1b[0m"]
    choice <- read <$> liftIO getLine :: Sh Int
    let !selected = options !! (choice-1)
    pure selected

giveOptions :: Text -> [Text] -> Sh [Text]
giveOptions optionName options = catchAskRetry $ do
    let indexedOptions = indexOptions options
    drawMenu indexedOptions
    echo $ conc ["Select multiple \x1b[31m",optionName,"\x1b[0m seperated by coma or \x1b[31mall\x1b[0m"]
    catchAskRetry $ do
      !choices <- T.splitOn "," <$> liftIO TIO.getLine
      if "all" `elem` choices then pure options
                              else pure $ snd <$> filter (\ (index,option) -> index `elem` fmap (read . T.unpack) choices) indexedOptions

getInput :: Text -> Sh Text
getInput label = do
    echo $ conc ["Enter value for \x1b[31m",label,"\x1b[0m"]
    liftIO TIO.getLine

grabDatabases :: RDSInstanceInfo -> Sh [Text]
grabDatabases RDSInstanceInfo{..} = cmd >>= parseNonDefaultDatabases
  where
    cmd = if handle == "local" then run' "kubectl" ["exec",pod,"--","mysql","-u",username,conc ["-p",password],"-e","show databases"]
                               else run' "kubectl" ["exec",pod,"--","mysql","-u",username,conc ["-p",password],"-h",handle,"-e","show databases"]

dumpDatabaseToFile :: RDSInstanceInfo -> Text -> Text -> Sh ()
dumpDatabaseToFile rdsInfo database outFile = do
    echo $ conc ["Dumping database \"",database,"\" to \"",outFile,"\"."]
    dumpDatabase rdsInfo database >>= writefile (fromText outFile)
  where
    dumpDatabase :: RDSInstanceInfo -> Text -> Sh Text
    dumpDatabase RDSInstanceInfo{..} database = cmd
      where
        cmd = if handle == "local" then run' "kubectl" ["exec",pod,"--","mysqldump","-u",username,conc ["-p",password],"--databases" ,database]
                                   else run' "kubectl" ["exec",pod,"--","mysqldump","-u",username,conc ["-p",password],"-h",handle,"--databases",database]
    

script :: Sh ()
script = run' "kubectl" ["config","get-contexts"]
    >>= parseClusterNames >>= giveOption "cluster"
    >>= \cluster -> run' "kubectl" ["config","use-context",cluster]
    >>= echo 
    >> echo "Scanning for mariadb pods"
    >> run' "kubectl" ["get","pods"] -|- run' "grep" ["mariadb"]
    >>= parsePods >>= giveOption "pod"
    >>= \pod -> catchAskRetry (getInput "master username" 
    >>= \username -> getInput "master password"
    >>= \password -> getInput "handle or 'local'"
    >>= \handle -> pure (RDSInstanceInfo pod handle username password)
    >>= \rdsInfo -> grabDatabases rdsInfo >>= \databases -> pure (rdsInfo,databases))
    >>= \(rdsInfo,allDatabases) -> giveOptions "databases" allDatabases
    >>= \databases -> echo "Dumping the following databases:"
    >> forM_ databases echo
    >> getInput "output folder"
    >>= \outFolder -> forM_ databases (\database -> dumpDatabaseToFile rdsInfo database (conc [outFolder,"/",database,".sql"]))  
    >> echo (conc ["Databases dumped successfully to ",(T.pack . show) outFolder,"."])
    >> pure ()

main :: IO ()
main = shelly $ print_stdout False script
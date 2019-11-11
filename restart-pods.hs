#!/usr/bin/env stack
-- stack script --resolver lts-13.17 --package shelly,text,exceptions --ghc-options -fno-warn-type-defaults

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE BangPatterns #-}

import Shelly
import Data.Text (Text)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM,forM_)
import Control.Monad.Catch (throwM)

default (Text)

conc = T.concat

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

run_' :: Text -> [Text] -> Sh ()
run_' program args = do
    liftIO $ do
        TIO.putStr $ conc ["[*] Running `\x1b[32m",program]
        forM_ args $ \arg -> TIO.putStr $ conc [" ",arg]
        TIO.putStrLn "\x1b[0m`"
    run_ (fromText program) args

parseClusterNames :: Text -> Sh [Text]
parseClusterNames input = do
    let xs = tail $ T.splitOn "\n" input
    fmap init <$> forM xs $ \line -> do
        let line' = T.dropWhile (\c -> isSpace c || c == '*') line
        pure $ T.takeWhile (not . isSpace) line'

parsePods :: Text -> Sh [Text]
parsePods input = do
    let xs = tail $ T.splitOn "\n" input
    fmap init <$> forM xs $ \line -> pure $ T.takeWhile (not . isSpace) line

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
    echo $ conc ["Input value for ",T.pack (show label),":"]
    liftIO TIO.getLine

restartPod :: Text -> Sh ()
restartPod pod = do
    echo $ conc ["Killing pod '",pod,"'."]
    run_' "kubectl" ["delete","pod",pod]

script :: Sh ()
script = run' "kubectl" ["config","get-contexts"]
    >>= parseClusterNames >>= giveOption "cluster" 
    >>= \cluster -> run' "kubectl" ["config","use-context",cluster] 
    >>= echo 
    >> echo "Scanning for pods.."
    >> run' "kubectl" ["get","pods"]
    >>= parsePods >>= giveOptions "pods"
    >>= \pods -> forM_ pods restartPod
    >> echo "Pods restarted."
    >> pure ()

main :: IO ()
main = shelly $ print_stdout False script
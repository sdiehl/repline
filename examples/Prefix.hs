{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main, repl) where

import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import System.Console.Repline

-------------------------------------------------------------------------------
-- Prefix Completion
-------------------------------------------------------------------------------

type Repl a = HaskelineT IO a

-- Evaluation
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Prefix tab completeter
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
  [ (":file", fileCompleter),
    (":holiday", listCompleter ["christmas", "thanksgiving", "festivus"])
  ]

-- Default tab completer
byWord :: Monad m => WordCompleter m
byWord n = do
  let names = ["picard", "riker", "data", ":file", ":holiday"]
  return $ filter (isPrefixOf n) names

files :: [String] -> Repl ()
files args = liftIO $ do
  contents <- readFile (unwords args)
  putStrLn contents

holidays :: [String] -> Repl ()
holidays [] = liftIO $ putStrLn "Enter a holiday."
holidays xs = liftIO $ do
  putStrLn $ "Happy " ++ unwords xs ++ "!"

opts :: [(String, [String] -> Repl ())]
opts =
  [ ("file", files),
    ("holiday", holidays)
  ]

inits :: Repl ()
inits = return ()

final :: Repl ExitDecision
final = return Exit

repl :: IO ()
repl = evalRepl (const $ pure ">>> ") cmd opts Nothing Nothing (Prefix (wordCompleter byWord) defaultMatcher) inits final

main :: IO ()
main = pure ()

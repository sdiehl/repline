{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import System.Console.Repline

import qualified Data.Set as Set
import Control.Monad.State.Strict

import Data.List (isPrefixOf)

-------------------------------------------------------------------------------
-- Stateful Completion
-------------------------------------------------------------------------------

type IState = Set.Set String
type Repl1 a = HaskelineT (StateT IState IO) a

-- Evaluation
cmd1 :: String -> Repl1 ()
cmd1 input = modify $ Set.insert input

-- Completion
completer1 :: (Monad m, MonadState IState m) => WordCompleter m
completer1 n = do
  ns <- get
  return $ filter (isPrefixOf n) (Set.toList ns)

-- Commands
help1 :: [String] -> Repl1 ()
help1 args = liftIO $ print $ "Help!" ++ show args

puts1 :: [String] -> Repl1 ()
puts1 args = modify $ Set.union (Set.fromList args)

opts1 :: [(String, [String] -> Repl1 ())]
opts1 = [
    ("help", help1) -- :help
  , ("puts", puts1) -- :puts
  ]

init1 :: Repl1 ()
init1 = return ()

-- Tab completion inside of StateT
repl1 :: IO ()
repl1 = flip evalStateT Set.empty
      $ evalRepl "_proto> " cmd1 opts1 (Word completer1) init1

-------------------------------------------------------------------------------
-- Command options
-------------------------------------------------------------------------------

type Repl2 a = HaskelineT IO a

-- Evaluation
cmd2 :: String -> Repl2 ()
cmd2 input = liftIO $ print input

-- Completion
comp2 :: Monad m => WordCompleter m
comp2 = listWordCompleter ["kirk", "spock", "mccoy"]

-- Commands
help2 :: [String] -> Repl2 ()
help2 args = liftIO $ print $ "Help!" ++ show args

opts2 :: [(String, [String] -> Repl2 ())]
opts2 = [
    ("help", help2)
  ]

init2 :: Repl2 ()
init2 = liftIO $ putStrLn "Welcome!"

repl2 :: IO ()
repl2 = evalRepl "example2> " cmd2 opts2 (Word comp2) init2

-------------------------------------------------------------------------------
-- Mixed Completion
-------------------------------------------------------------------------------

type Repl3 a = HaskelineT IO a

-- Evaluation
cmd3 :: String -> Repl2 ()
cmd3 input = liftIO $ print input

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":file"    , fileCompleter)
  , (":holiday" , listCompleter ["christmas", "thanksgiving", "festivus"])
  ]

byWord :: Monad m => WordCompleter m
byWord n = do
  let names = ["picard", "riker", "data", ":file", ":holiday"]
  return $ filter (isPrefixOf n) names

files :: [String] -> Repl2 ()
files args = liftIO $ do
  contents <- readFile (unwords args)
  putStrLn contents

holidays :: [String] -> Repl2 ()
holidays [] = liftIO $ putStrLn "Enter a holiday."
holidays xs = liftIO $ do
  putStrLn $ "Happy " ++ unwords xs ++ "!"

opts3 :: [(String, [String] -> Repl2 ())]
opts3 = [
    ("file", files)
  , ("holiday", holidays)
  ]

init3 :: Repl3 ()
init3 = return ()

repl3 :: IO ()
repl3 = evalRepl "example3> " cmd3 opts3 (Prefix (wordCompleter byWord) defaultMatcher) init3

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

main :: IO ()
main = repl1 >> repl2 >> repl3

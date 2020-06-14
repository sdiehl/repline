{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import System.Console.Repline

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
opts1 =
  [ ("help", help1), -- :help
    ("puts", puts1) -- :puts
  ]

init1 :: Repl1 ()
init1 = return ()

final1 :: Repl1 ExitDecision
final1 = return Exit

-- Tab completion inside of StateT
repl1 :: IO ()
repl1 =
  flip evalStateT Set.empty $
    evalRepl (const $ pure "_proto> ") cmd1 opts1 (Just ':') Nothing (Word completer1) init1 final1

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
opts2 =
  [ ("help", help2)
  ]

init2 :: Repl2 ()
init2 = liftIO $ putStrLn "Welcome!"

final2 :: Repl2 ExitDecision
final2 = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

repl2 :: IO ()
repl2 = evalRepl (const $ pure "example2> ") cmd2 opts2 (Just ':') Nothing (Word comp2) init2 final2

-------------------------------------------------------------------------------
-- Mixed Completion
-------------------------------------------------------------------------------

type Repl3 a = HaskelineT IO a

-- Evaluation
cmd3 :: String -> Repl3 ()
cmd3 input = liftIO $ print input

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher =
  [ (":file", fileCompleter),
    (":holiday", listCompleter ["christmas", "thanksgiving", "festivus"])
  ]

byWord :: Monad m => WordCompleter m
byWord n = do
  let names = ["picard", "riker", "data", ":file", ":holiday"]
  return $ filter (isPrefixOf n) names

files :: [String] -> Repl3 ()
files args = liftIO $ do
  contents <- readFile (unwords args)
  putStrLn contents

holidays :: [String] -> Repl3 ()
holidays [] = liftIO $ putStrLn "Enter a holiday."
holidays xs = liftIO $ do
  putStrLn $ "Happy " ++ unwords xs ++ "!"

opts3 :: [(String, [String] -> Repl3 ())]
opts3 =
  [ ("file", files),
    ("holiday", holidays)
  ]

init3 :: Repl3 ()
init3 = return ()

final3 :: Repl3 ExitDecision
final3 = return Exit

repl3 :: IO ()
repl3 = evalRepl (const $ pure "example3> ") cmd3 opts3 (Just ':') Nothing (Prefix (wordCompleter byWord) defaultMatcher) init3 final3

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

main :: IO ()
main = repl1 >> repl2 >> repl3

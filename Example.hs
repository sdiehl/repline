{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import System.Console.Repline
import System.Console.Haskeline.Completion

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
cmd1 input = modify $ \s -> Set.insert input s

-- Completion
completer1 :: (Monad m, MonadState IState m) => WordCompleter m
completer1 n = do
  ns <- get
  let matches = filter (isPrefixOf n) (Set.toList ns)
  return $ map simpleCompletion matches

-- Commands
help1 :: [String] -> Repl1 ()
help1 args = liftIO $ print $ "Help!" ++ show args

puts1 :: [String] -> Repl1 ()
puts1 args = modify $ \s -> Set.union s (Set.fromList args)

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
-- Flat IO
-------------------------------------------------------------------------------

type Repl2 a = HaskelineT IO a

-- Evaluation
cmd2 :: String -> Repl2 ()
cmd2 input = liftIO $ print input

-- Completion
completer2 :: Monad m => WordCompleter m
completer2 n = do
  let names = ["kirk", "spock", "mccoy"]
  let matches = filter (isPrefixOf n) names
  return $ map simpleCompletion matches

-- Commands
help2 :: [String] -> Repl2 ()
help2 args = liftIO $ print $ "Help!" ++ show args

myOptions2 :: [(String, [String] -> Repl2 ())]
myOptions2 = [
    ("help", help2)
  ]

init2 :: Repl2 ()
init2 = liftIO $ putStrLn "Welcome!"

repl2 :: IO ()
repl2 = evalRepl "example2> " cmd2 myOptions2 (Word completer2) init2

-------------------------------------------------------------------------------
-- Mixed Completion
-------------------------------------------------------------------------------

type Repl3 a = HaskelineT IO a

-- Evaluation
cmd3 :: String -> Repl2 ()
cmd3 input = liftIO $ print input

-- Completion
completer3 :: MonadIO m => CompletionFunc m
completer3 (start, n) =
  if ":file" `isPrefixOf` (reverse start)
  then do
    case words (reverse start) of
      (":file": xs) -> do
        matches <- listFiles (concat xs)
        return $ (start, map (trimComplete (concat xs)) matches)
      _ -> do
        matches <- listFiles ""
        return $ (start, matches)
  else do
    let names = ["kirk", "spock", "mccoy"]
    let matches = filter (isPrefixOf (reverse start)) names
    return $ (start, map (trimComplete start) (map simpleCompletion matches))

-- Commands
file3 :: [String] -> Repl2 ()
file3 args = liftIO $ do
  contents <- readFile (unwords args)
  putStrLn contents

myOptions3 :: [(String, [String] -> Repl2 ())]
myOptions3 = [
    ("file", file3)
  ]

init3 :: Repl3 ()
init3 = return ()

repl3 :: IO ()
repl3 = evalRepl "example3> " cmd3 myOptions3 (Mixed completer3) init3

main :: IO ()
main = repl2

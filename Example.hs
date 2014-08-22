{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Example where

import Console

import qualified Data.Set as Set
import Control.Monad.State.Strict

import Data.List (isPrefixOf)
import System.Console.Haskeline.Completion

-------------------------------------------------------------------------------
-- Example1
-------------------------------------------------------------------------------

type IState = Set.Set String
type Repl1 a = HaskelineT (StateT IState IO) a

-- Evaluation
cmd1 :: String -> Repl1 ()
cmd1 input = modify $ \s -> Set.insert input s

-- Completion
completer1 :: MonadState IState m => String -> m [Completion]
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

-- Tab completion inside of StateT
main1 :: IO ()
main1 = flip evalStateT Set.empty
      $ evalRepl "_proto> " cmd1 opts1 completer1

-------------------------------------------------------------------------------
-- Example2
-------------------------------------------------------------------------------

type Repl2 a = HaskelineT IO a

-- Evaluation
cmd2 :: String -> Repl2 ()
cmd2 input = liftIO $ print input

-- Completion
completer2 :: Monad m => String -> m [Completion]
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

-- Flat IO.
main2 :: IO ()
main2 = evalRepl "example2> " cmd2 myOptions2 completer2

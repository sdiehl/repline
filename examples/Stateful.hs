{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main, repl) where

import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import System.Console.Repline

-------------------------------------------------------------------------------
-- Stateful Completion
-------------------------------------------------------------------------------

type IState = Set.Set String

type Repl a = HaskelineT (StateT IState IO) a

-- Evaluation
cmd :: String -> Repl ()
cmd input = modify $ \s -> Set.insert input s

-- Completion
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  ns <- get
  return $ filter (isPrefixOf n) (Set.toList ns)

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help!" ++ show args

puts :: [String] -> Repl ()
puts args = modify $ \s -> Set.union s (Set.fromList args)

opts :: [(String, [String] -> Repl ())]
opts =
  [ ("help", help), -- :help
    ("puts", puts) -- :puts
  ]

ini :: Repl ()
ini = return ()

-- Tab completion inside of StateT
repl :: IO ()
repl =
  flip evalStateT Set.empty $
    evalRepl (pure ">>> ") cmd opts Nothing (Word comp) ini

main :: IO ()
main = pure ()

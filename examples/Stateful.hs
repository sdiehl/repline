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

type IState = (Int, Set.Set String)

type Repl a = HaskelineT (StateT IState IO) a

-- Evaluation
cmd :: String -> Repl ()
cmd input = modify . fmap $ \s -> Set.insert input s

-- Completion
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  (c, ns) <- get
  return $ filter (isPrefixOf n) (Set.toList ns)

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help!" ++ show args

puts :: [String] -> Repl ()
puts args = modify . fmap $ \s -> Set.union s (Set.fromList args)

opts :: [(String, [String] -> Repl ())]
opts =
  [ ("help", help), -- :help
    ("puts", puts) -- :puts
  ]

ini :: Repl ()
ini = return ()

final :: Repl ExitDecision
final = do
  (count, s) <- get
  if count == 0
  then return Exit
  else do
    liftIO . putStrLn $ "Exit in " <> show count <> "..."
    put (count - 1, s)
    return Continue

-- Tab completion inside of StateT
repl :: IO ()
repl =
  flip evalStateT (3, Set.empty) $
    evalRepl (const $ pure ">>> ") cmd opts Nothing Nothing (Word comp) ini final

main :: IO ()
main = pure ()

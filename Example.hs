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
-- Instance
-------------------------------------------------------------------------------

type IState = Set.Set String
type Repl a = HaskelineT (StateT IState IO) a

-- Evaluation
myCmd :: String -> Repl ()
myCmd input = do
  modify $ \s -> Set.insert input s

-- Completion
myComplete :: MonadState IState m => String -> m [Completion]
myComplete n = do
  ns <- get
  let matches = filter (isPrefixOf n) (Set.toList ns)
  return $ map simpleCompletion matches

-- Commands
cmdHelp :: [String] -> Repl ()
cmdHelp args = do
  liftIO $ print $ "Help!" ++ show args

cmdPut :: [String] -> Repl ()
cmdPut args = do
  modify $ \s -> Set.fromList ["Haskell"]

myOptions :: [(String, [String] -> Repl ())]
myOptions = [
    ("help", cmdHelp)
  , ("put",  cmdPut)
  ]

main :: IO ()
main = flip evalStateT Set.empty
     $ evalRepl "_proto> " myCmd myOptions myComplete

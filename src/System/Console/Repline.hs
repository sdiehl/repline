{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Console.Repline (
  HaskelineT,
  runHaskelineT,

  Cmd,
  Options,
  Completer,
  Command,

  ReplSettings(..),

  evalRepl,
  evalReplWith,
) where

import System.Console.Haskeline.Completion
import System.Console.Haskeline.MonadException
import qualified System.Console.Haskeline as H

import Data.List (isPrefixOf)
import Control.Applicative
import Control.Monad.State.Strict

-------------------------------------------------------------------------------
-- Haskeline Transformer
-------------------------------------------------------------------------------

newtype HaskelineT m a = HaskelineT {unHaskeline :: H.InputT m a}
 deriving (Monad, Functor, Applicative, MonadIO, MonadException, MonadTrans, MonadHaskeline)

runHaskelineT :: MonadException m => H.Settings m -> HaskelineT m a -> m a
runHaskelineT s m = H.runInputT s (H.withInterrupt (unHaskeline m))

class MonadException m => MonadHaskeline m where
  getInputLine :: String -> m (Maybe String)
  getInputChar :: String -> m (Maybe Char)
  outputStr    :: String -> m ()
  outputStrLn  :: String -> m ()

instance MonadException m => MonadHaskeline (H.InputT m) where
  getInputLine = H.getInputLine
  getInputChar = H.getInputChar
  outputStr    = H.outputStr
  outputStrLn  = H.outputStrLn

instance MonadState s m => MonadState s (HaskelineT m) where
  get = lift get
  put = lift . put

instance (MonadHaskeline m) => MonadHaskeline (StateT s m) where
  getInputLine = lift . getInputLine
  getInputChar = lift . getInputChar
  outputStr    = lift . outputStr
  outputStrLn  = lift . outputStrLn

-------------------------------------------------------------------------------
-- Repl
-------------------------------------------------------------------------------

type Cmd m = [String] -> m ()
type Options m = [(String, Cmd m)]
type Completer m = (String -> m [Completion])
type Command m = String -> m ()

data ReplSettings m = ReplSettings
  { _cmd      :: Command (HaskelineT m)
  , _opts     :: Options (HaskelineT m)
  , _complete :: Completer m
  , _init     :: m ()
  , _banner   :: String
  }

tryAction :: MonadException m => HaskelineT m a -> HaskelineT m a
tryAction (HaskelineT f) = HaskelineT (H.withInterrupt loop)
    where loop = handle (\H.Interrupt -> loop) f

replLoop :: MonadException m
         => String
         -> Command (HaskelineT m)
         -> Options (HaskelineT m)
         -> HaskelineT m ()
replLoop banner cmdM opts = loop
  where
    loop = tryAction $ do
      minput <- getInputLine banner
      case minput of
        Nothing -> outputStrLn "Goodbye."

        Just ":" -> do
          loop

        Just (':' : cmds) -> do
          let (cmd:args) = words cmds
          optMatcher cmd opts args
          loop

        Just input -> do
          cmdM input
          loop

optMatcher :: MonadHaskeline m => String -> Options m -> [String] -> m ()
optMatcher s [] _ = outputStrLn $ "No such command :" ++ s
optMatcher s ((x, m):xs) args
  | s `isPrefixOf` x = m args
  | otherwise = optMatcher s xs args


evalRepl :: MonadException m             -- ^ Terminal monad ( often IO ).
         => String                       -- ^ Banner
         -> Command (HaskelineT m)       -- ^ Command function
         -> Options (HaskelineT m)       -- ^ Options list and commands
         -> (String -> m [Completion])   -- ^ Tab completion function
         -> m ()
evalRepl banner cmd opts comp = runHaskelineT _readline (tryAction monad)
  where
    monad = replLoop banner cmd opts
    _readline = H.Settings
      { H.complete       = completeWord Nothing "  \t()[]" comp
      , H.historyFile    = Just ".history"
      , H.autoAddHistory = True
      }


evalReplWith :: MonadException m => ReplSettings m -> m ()
evalReplWith settings = runHaskelineT _readline (tryAction monad)
  where
    monad = replLoop (_banner settings) (_cmd settings) (_opts settings)
    _readline = H.Settings
      { H.complete       = completeWord Nothing "  \t()[]" (_complete settings)
      , H.historyFile    = Just ".history"
      , H.autoAddHistory = True
      }

{-# LANGUAGE KindSignatures #-}
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
  WordCompleter,
  LineCompleter,
  CompleterStyle(..),
  Command,

  ReplSettings(ReplSettings),
  emptySettings,

  evalRepl,
  evalReplWith,

  listFiles, -- re-export
  trimComplete,
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

newtype HaskelineT (m :: * -> *) a = HaskelineT {unHaskeline :: H.InputT m a}
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
type Command m = String -> m ()

type WordCompleter m = (String -> m [Completion])
type LineCompleter m = (String -> String -> m [Completion])

data ReplSettings m = ReplSettings
  { _cmd       :: Command (HaskelineT m)
  , _opts      :: Options (HaskelineT m)
  , _compstyle :: CompleterStyle m
  , _init      :: m ()
  , _banner    :: String
  }

emptySettings :: MonadHaskeline m => ReplSettings m
emptySettings = ReplSettings (const $ return ()) [] File (return ()) ""

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
    loop = do
      minput <- H.handleInterrupt (return (Just "")) $ getInputLine banner
      case minput of
        Nothing -> outputStrLn "Goodbye."

        Just "" -> loop
        Just ":" -> loop

        Just (':' : cmds) -> do
          let (cmd:args) = words cmds
          optMatcher cmd opts args
          loop

        Just input -> do
          H.handleInterrupt (liftIO $ putStrLn "Ctrl-C") $ cmdM input
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
         -> CompleterStyle m             -- ^ Tab completion function
         -> HaskelineT m a               -- ^ Initializer
         -> m ()
evalRepl banner cmd opts comp initz = runHaskelineT _readline (initz >> monad)
  where
    monad = replLoop banner cmd opts
    _readline = H.Settings
      { H.complete       = mkCompleter comp
      , H.historyFile    = Just ".history"
      , H.autoAddHistory = True
      }

evalReplWith :: MonadException m => ReplSettings m -> m ()
evalReplWith settings = runHaskelineT _readline (tryAction monad)
  where
    monad = replLoop (_banner settings) (_cmd settings) (_opts settings)
    _readline = H.Settings
      { H.complete       = mkCompleter (_compstyle settings)
      , H.historyFile    = Just ".history"
      , H.autoAddHistory = True
      }

-------------------------------------------------------------------------------
-- Completions
-------------------------------------------------------------------------------

data CompleterStyle m
  = Word (WordCompleter m)
  | Cursor (LineCompleter m)
  | File
  | Mixed (CompletionFunc m)

mkCompleter :: MonadIO m => CompleterStyle m -> CompletionFunc m
mkCompleter (Word f)   = completeWord (Just '\\') " \t()[]" f
mkCompleter (Cursor f) = completeWordWithPrev (Just '\\') " \t()[]" f
mkCompleter (Mixed f)  = f
mkCompleter File       = completeFilename

trimComplete :: String -> Completion -> Completion
trimComplete prefix (Completion a b c) = Completion (drop (length prefix) a) b c

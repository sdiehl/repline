Repline
-------

[![Build Status](https://travis-ci.org/sdiehl/repline.svg)](https://travis-ci.org/sdiehl/repline)
[![Hackage](https://img.shields.io/hackage/v/repline.svg)](https://hackage.haskell.org/package/repline)

Slightly higher level wrapper for creating GHCi-like REPL monads that are composable with normal MTL
transformers. Mostly exists because I got tired of implementing the same interface for simple shells over and
over and decided to canonize the giant pile of hacks that I use to make Haskeline work.

Usage
-----

```haskell
type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

say :: [String] -> Repl ()
say args = do
  _ <- liftIO $ system $ "cowsay" ++ " " ++ (unwords args)
  return ()

options :: [(String, [String] -> Repl ())]
options = [
    ("help", help)  -- :help
  , ("say", say)    -- :say
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

repl :: IO ()
repl = evalRepl ">>> " cmd options (Word completer) ini
```

Trying it out:

```haskell
$ runhaskell Main.hs
Welcome!
>>> <TAB>
kirk spock mccoy

>>> k<TAB>
kirk

>>> spam
"spam"

>>> :say Hello Haskell
 _______________ 
< Hello Haskell >
 --------------- 
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```


Stateful Tab Completion
-----------------------

```haskell
type IState = Set.Set String
type Repl a = HaskelineT (StateT IState IO) a

-- Evaluation
cmd :: String -> Repl ()
cmd input = modify $ \s -> Set.insert input s

-- Completion
comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  ns <- get
  return  $ filter (isPrefixOf n) (Set.toList ns)

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help!" ++ show args

puts :: [String] -> Repl ()
puts args = modify $ \s -> Set.union s (Set.fromList args)

opts :: [(String, [String] -> Repl ())]
opts = [
    ("help", help) -- :help
  , ("puts", puts) -- :puts
  ]

ini :: Repl ()
ini = return ()

-- Tab completion inside of StateT
repl :: IO ()
repl = flip evalStateT Set.empty
      $ evalRepl "_proto> " cmd opts (Word comp) init
```

Mixed Completion
----------------

Installation
------------

```bash
$ cabal install repline
```

License
-------

Copyright (c) 2014, Stephen Diehl
Released under the MIT License

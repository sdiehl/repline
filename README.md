Repline
-------

[![Build Status](https://travis-ci.org/sdiehl/repline.svg)](https://travis-ci.org/sdiehl/repline)
[![Hackage](https://img.shields.io/hackage/v/repline.svg)](https://hackage.haskell.org/package/repline)

Slightly higher level wrapper for creating GHCi-like REPL monads that are composable with normal MTL
transformers. Mostly exists because I got tired of implementing the same interface for simple shells over and
over, and because vanilla Haskeline has a kind of quirky API.


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
  let matches = filter (isPrefixOf n) names
  return $ map simpleCompletion matches

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

main :: IO ()
main = evalRepl ">>> " cmd options (Word completer) ini
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

Installation
------------

```bash
$ cabal install repline
```

License
-------

Copyright (c) 2014, Stephen Diehl
Released under the MIT License

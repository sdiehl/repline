Repline
-------

[![Build Status](https://travis-ci.org/sdiehl/repline.svg?branch=master)](https://travis-ci.org/sdiehl/repline)
[![Hackage](https://img.shields.io/hackage/v/repline.svg)](https://hackage.haskell.org/package/repline)

Slightly higher level wrapper for creating GHCi-like REPL monads that are composable with normal MTL
transformers. Mostly exists because I got tired of implementing the same interface for simple shells over and
over and decided to canonize the giant pile of hacks that I use to make Haskeline work.

See
[Documentation](https://hackage.haskell.org/package/repline-0.2.2.0/docs/System-Console-Repline.html)
for more detailed usage.

Examples
--------

* [Simple](examples/Simple.hs)
* [Prefix](examples/Prefix.hs)
* [Stateful](examples/Stateful.hs)
* [Multiline](examples/MultiLine.hs)

Migration from 0.3.x
--------------------

This release adds two parameters to the `ReplOpts` constructor and `evalRepl` function.

* `finaliser`
* `multilineCommand`

The `finaliser` function is a function run when the Repl monad is is exited.

```haskell
-- | Decide whether to exit the REPL or not
data ExitDecision
  = Continue -- | Keep the REPL open
  | Exit     -- | Close the REPL and exit
```

For example:

```haskell
final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit
```

The `multilineCommand` argument takes a command which invokes a multiline edit
mode in which the user can paste/enter text across multiple lines terminating
with a Ctrl-D / EOF. This can be used in conjunction with a customBanner
function to indicate the entry mode.

```haskell
customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure ">>> "
customBanner MultiLine = pure "| "
```

See [Multiline](examples/MultiLine.hs) for a complete example.

Migration from 0.2.x
--------------------

The underlying `haskeline` library that provides readline support had a breaking
API change in 0.8.0.0 which removed the bespoke
`System.Console.Haskeline.MonadException` module in favour of using the
`exceptions` package. This is a *much* better design and I strongly encourage
upgrading. To migrate simply add the following bounds to your Cabal file.

```yaml
build-depends:
  repline   >= 0.3.0.0
  haskeline >= 0.8.0.0
```

You may also need to add the following to your `stack.yaml` file if using Stack.

```yaml
resolver: lts-15.0
packages:
  - .
extra-deps:
  - haskeline-0.8.0.0
  - repline-0.3.0.0
```

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
repl = evalRepl (pure ">>> ") cmd options Nothing (Word completer) ini
```

Trying it out:

```haskell
$ stack repl Simple.hs
Prelude> main

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

Quite often tab completion is dependent on the internal state of the Repl so we'd like to query state of the
interpreter for tab completions based on actions performed themselves within the Repl, this is modeleted
naturally as a monad transformer stack with ``StateT`` on top of ``HaskelineT``.

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
     $ evalRepl (pure ">>> ") cmd opts Nothing (Word comp) ini
```


Prefix Completion
-----------------

Just as GHCi will provide different tab completion for kind-level vs type-level symbols based on which prefix
the user has entered, we can also set up a provide this as a first-level construct using a ``Prefix`` tab
completer which takes care of the string matching behind the API.

```haskell
type Repl a = HaskelineT IO a

-- Evaluation
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Prefix tab completeter
defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":file"    , fileCompleter)
  , (":holiday" , listCompleter ["christmas", "thanksgiving", "festivus"])
  ]

-- Default tab completer
byWord :: Monad m => WordCompleter m
byWord n = do
  let names = ["picard", "riker", "data", ":file", ":holiday"]
  return $ filter (isPrefixOf n) names

files :: [String] -> Repl ()
files args = liftIO $ do
  contents <- readFile (unwords args)
  putStrLn contents

holidays :: [String] -> Repl ()
holidays [] = liftIO $ putStrLn "Enter a holiday."
holidays xs = liftIO $ do
  putStrLn $ "Happy " ++ unwords xs ++ "!"

opts :: [(String, [String] -> Repl ())]
opts = [
    ("file", files)
  , ("holiday", holidays)
  ]

init :: Repl ()
init = return ()

repl :: IO ()
repl = evalRepl (pure ">>> ") cmd opts Nothing (Prefix (wordCompleter byWord) defaultMatcher) init
```

Trying it out:

```haskell
$ stack repl examples/Prefix.hs
Prelude> main

>>> :file <TAB>
sample1.txt sample2.txt

>>> :file sample1.txt

>>> :holiday <TAB>
christmas thanksgiving festivus
```

License
-------

Copyright (c) 2014-2020, Stephen Diehl
Released under the MIT License

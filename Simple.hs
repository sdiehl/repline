import Control.Monad.Trans
import System.Console.Repline
import System.Console.Haskeline.Completion

import System.Cmd
import Data.List (isPrefixOf)

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

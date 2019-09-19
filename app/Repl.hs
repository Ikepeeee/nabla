module Repl where

import Control.Monad.IO.Class
import Control.Monad (when)
import System.Exit (exitSuccess, exitFailure)
import Text.Megaparsec (parse)
import Nabla.Parser (ast)
import Nabla.Executor (exec)
import System.Console.Haskeline
import Nabla.AST

main :: IO ()
main = runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  line <- getInputLine "nabla> "
  case line of
    Nothing -> liftIO exitFailure
    Just l -> do
      when (l == ".exit") $ liftIO $ putStrLn "ByeBye~(^q^)" >> exitSuccess
      case astParse l of
        Right ast -> liftIO $ exec ast
        Left e -> do
          liftIO $ print e
          pure $ Left $ show e
      repl

astParse line = parse ast "repl" (line <> "\n")

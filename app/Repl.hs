{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving  #-}

module Repl where

import Control.Monad.IO.Class
import Control.Monad (when)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Nabla.Parser (ast)
import Nabla.Executor (exec, runExecutor, ValueState(ValueState))
import System.Console.Haskeline
import Nabla.IST (Value)
import Nabla.Fixture (typeVars)

main :: IO ()
main = repl $ ValueState [] typeVars []

newtype REPL a = REPL (StateT ValueState (ExceptT String IO) a)
  deriving (Functor, Applicative, Monad, MonadState ValueState, MonadError String)

runREPL :: REPL a -> ValueState -> IO (Either String (a, ValueState))
runREPL (REPL repl) vars = runExceptT (runStateT repl vars)

repl :: ValueState -> IO ()
repl vars = runInputT defaultSettings (repl' vars)
  where
    repl' vars = do
      line' <- getInputLine "nabla> "
      line <- case line' of
        Nothing -> return ""
        Just line -> return line
      when (line == ".exit") $ liftIO $ putStrLn "ByeBye~(^q^)" >> exitSuccess
      res <- liftIO $ runREPL (replOneExpr line) vars
      newVars <- case res of
        Right (values, vars') -> do
          lift $ putStrLn $ intercalate "\n" $ map show values
          return vars'
        Left e -> do
          lift $ putStrLn e
          return vars
      repl' newVars

replOneExpr :: String -> REPL [Value]
replOneExpr line = do
  state <- get
  ast <- case astParse line of
    Right ast -> return ast
    Left e -> throwError $ errorBundlePretty e
  (vs, state) <- case runExecutor (exec ast) state of
    Right res -> return res
    Left e -> throwError e
  put state
  return vs

astParse line = parse ast "repl" (line <> "\n")

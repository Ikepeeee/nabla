{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Nabla.Executor where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Nabla.AST

newtype Executor a = Executor (StateT [Var] (ExceptT String Identity) a)
  deriving (Functor, Applicative, Monad, MonadState [Var], MonadError String)

runExecutor :: Executor a -> [Var] -> Either String (a, [Var])
runExecutor (Executor exec) vars = runIdentity (runExceptT (runStateT exec vars))

type Var = (Identifier, Value)

exec :: AST -> Executor [Value]
exec (AST []) = return []
exec (AST (expr:exprs)) = (:) <$> (eval expr) <*> exec (AST exprs)

eval :: Expr -> Executor Value
eval (ValueExpr v) = return v
eval (VariableExpr name) = do
  vars <- get
  case lookup name vars of
    Nothing -> throwError $ name <> " is not assign yet."
    Just v -> return v
eval (Assign name v) = do
  vars <- get
  case lookup name vars of
    Just _ -> throwError $ name <> " is already assign."
    Nothing -> return v
  v' <- eval v
  put $ (name, v'):vars
  return v'
eval (Complex (WrapValues c exprs)) = do
  vs <- evalMany exprs
  return $ ComplexV $ WrapValues c vs
  where
    evalMany [] = pure []
    evalMany (e:es) = (:) <$> (eval e) <*> (evalMany es)

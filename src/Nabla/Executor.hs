{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Nabla.Executor where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import qualified Nabla.AST as AST
import qualified Nabla.IST as IST

newtype Executor a = Executor (StateT [IST.Var] (ExceptT String Identity) a)
  deriving (Functor, Applicative, Monad, MonadState [IST.Var], MonadError String)

runExecutor :: Executor a -> [IST.Var] -> Either String (a, [IST.Var])
runExecutor (Executor exec) vars = runIdentity (runExceptT (runStateT exec vars))

exec :: AST.AST -> Executor [IST.Value]
exec (AST.AST []) = return []
exec (AST.AST (expr:exprs)) = (:) <$> (eval expr) <*> exec (AST.AST exprs)

eval :: AST.Expr -> Executor IST.Value
eval (AST.ValueExpr (AST.SimpleV (AST.StringV v))) = pure (IST.StringV v)
eval (AST.ValueExpr (AST.SimpleV (AST.NumberV v))) = pure (IST.NumberV v)
eval (AST.ValueExpr (AST.SimpleV (AST.SymbolV v))) = pure (IST.SymbolV v)
eval (AST.ValueExpr (AST.ComplexV (AST.WrapValues c es))) = IST.ComplexV c <$> (mapM eval es)
eval (AST.VariableExpr name) = do
  vars <- get
  case lookup name vars of
    Nothing -> throwError $ name <> " is not assign yet."
    Just v -> return v
eval (AST.Assign name v) = do
  vars <- get
  case lookup name vars of
    Just _ -> throwError $ name <> " is already assign."
    Nothing -> return v
  v' <- eval v
  put $ (name, v'):vars
  return v'

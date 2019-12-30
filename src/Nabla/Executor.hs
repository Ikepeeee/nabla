{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Nabla.Executor where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Data.List (find, intercalate)
import qualified Nabla.AST as AST
import qualified Nabla.IST as IST

data ValueState = ValueState {
  vars :: [IST.Var],
  types :: [IST.Type],
  signatures :: [IST.Signature]
}

addVars :: IST.Var -> ValueState -> ValueState
addVars v vs = ValueState (v:(vars vs)) (types vs) (signatures vs)
addTypes :: IST.Type -> ValueState -> ValueState
addTypes tv vs = ValueState (vars vs) (tv:(types vs)) (signatures vs)
addTypedVars :: IST.Signature -> ValueState -> ValueState
addTypedVars tv vs = ValueState (vars vs) (types vs) (tv:(signatures vs))

newtype Executor a = Executor (StateT ValueState (ExceptT String Identity) a)
  deriving (Functor, Applicative, Monad, MonadState ValueState, MonadError String)

runExecutor :: Executor a -> ValueState -> Either String (a, ValueState)
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
  vs <- get
  case lookup name (vars vs) of
    Nothing -> throwError $ name <> " is not assign yet."
    Just v -> return v
eval (AST.Assign name v) = do
  vs <- get
  case lookup name (vars vs) of
    Just _ -> throwError $ name <> " is already assign."
    Nothing -> return v
  v' <- eval v
  case lookup name (signatures vs) of
    Just ts -> case IST.infer v' ts of
      [] -> throwError $ name <> " should be " <> intercalate " | " (map IST.typeName ts)
      _ -> return ()
    Nothing -> return ()
  modify $ addVars (name, v')
  modify $ addTypedVars (name, IST.infer v' (types vs))
  return v'
eval (AST.TypeAssign name (AST.TypeName t)) = do
  vs <- get
  case lookup name (signatures vs) of
    Just _ -> throwError $ name <> " is already defined."
    Nothing -> return ()
  case find (\(IST.Type n _) -> n == t) (types vs) of
    Just tp -> modify $ addTypedVars (name, [tp])
    Nothing -> throwError $ t <> " is not defined."
  return $ IST.ComplexV "Nothing" []

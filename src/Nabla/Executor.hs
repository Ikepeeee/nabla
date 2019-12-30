{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Nabla.Executor where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import qualified Nabla.AST as AST
import qualified Nabla.IST as IST

data ValueState = ValueState {
  vars :: [IST.Var],
  types :: [IST.TypeVar],
  typedVars :: [IST.TypedVar]
}

addVars :: IST.Var -> ValueState -> ValueState
addVars v vs = ValueState (v:(vars vs)) (types vs) (typedVars vs)
addTypes :: IST.TypeVar -> ValueState -> ValueState
addTypes tv vs = ValueState (vars vs) (tv:(types vs)) (typedVars vs)
addTypedVars :: IST.TypedVar -> ValueState -> ValueState
addTypedVars tv vs = ValueState (vars vs) (types vs) (tv:(typedVars vs))

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
  case lookup name (typedVars vs) of
    Just ts -> case all (\t -> t v') ts of
      True -> return ()
      False -> throwError $ name <> " can't be assign because of type error."
    Nothing -> return ()
  modify $ addVars (name, v')
  modify $ addTypedVars (name, IST.infer v' (map snd (types vs)))
  return v'
eval (AST.TypeAssign name (AST.TypeName t)) = do
  vs <- get
  case lookup name (typedVars vs) of
    Just _ -> throwError $ name <> " is already defined."
    Nothing -> return ()
  case lookup t (types vs) of
    Just tp -> modify $ addTypedVars (name, [tp])
    Nothing -> throwError $ t <> " is not defined."
  return $ IST.ComplexV "Nothing" []

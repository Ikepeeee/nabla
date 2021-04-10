module Language.Nabla.TypeChecker where

import Prelude hiding (lookup)
import Control.Monad.State
import Data.Map (empty, fromList, insert, lookup, Map)
import Language.Nabla.AST

-- Type Environment
type TypeEnv = Map String Type

infer :: TypeEnv -> Expr -> Type
infer _ (Num _) = TNum
infer _ (Bool _) = TBool
infer env (Var x) = do
  case lookup x env of
    Just t  -> t
    Nothing -> error ("not found: " ++ x)
infer env (Fun parm t e) = TFun t (infer (insert parm t env) e)
infer env (App f arg) = typeApp (infer env f) (infer env arg)

typeApp :: Type -> Type -> Type
typeApp (TFun t r) t'
  | t == t' = r
  | otherwise = error $ show t' <> " can't apply " <> show t


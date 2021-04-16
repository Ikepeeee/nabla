module Language.Nabla.TypeChecker where
import Prelude hiding (lookup)
import Data.SBV.Dynamic
import Control.Monad.State
import Data.Map (empty, fromList, insert, lookup, Map)
import Language.Nabla.AST

-- Type Environment
type TypeEnv = Map String Type

valid :: TypeEnv -> TypedExpr -> Either String Type
valid env (TypedExpr e Nothing) = infer env e
valid env (TypedExpr e (Just t)) = do
  t' <- infer env e
  if t == t'
    then pure t
    else Left $ show e <> " is not " <> show t

infer :: TypeEnv -> Expr -> Either String Type
infer _ (Num _) = pure TNum
infer _ (Bool _) = pure TBool
infer env (Var x) = do
  case lookup x env of
    Just t  -> pure t
    Nothing -> Left ("not found: " ++ x)
infer env (Fun parm t e) = TFun t <$> infer (insert parm t env) e
infer env (App f arg) = join $ typeApp <$> infer env f <*> infer env arg

typeApp :: Type -> Type -> Either String Type
typeApp tf@(TFun t r) t'
  | t == t' = pure r
  | otherwise = Left $ "can't apply: (" <> show tf <> ") " <> show t'


-- toSVal :: Expr -> SVal
-- toSVal (Num e) = svInteger KFloat e
-- toSVal (Bool e) = svBool e
-- toSVal (App (App (Var "+") v1) v2) = svPlus (toSVal v1) (toSVal v2)
-- toSVal (App (App (Var ">") v1) v2) = svGreaterThan (toSVal v1) (toSVal v2)


module Language.Nabla.TypeChecker where
import Prelude hiding (lookup)
import Data.SBV (symbolic)
import Data.SBV.Dynamic
import Control.Monad.State
import Data.Map (empty, fromList, insert, lookup, Map, member, (!))
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

data SBVExpr
  = SBVVal SVal
  | SBVFun (SVal -> SBVExpr)

instance Show SBVExpr where
  show (SBVVal _) = "SBVVal"
  show (SBVFun _) = "SBVFun"

sbvBin :: (SVal -> SVal -> SVal) -> SBVExpr
sbvBin bin = SBVFun $ \a -> SBVFun $ \b -> SBVVal $ bin a b

toSBVExpr :: Map String SVal -> Expr -> Symbolic SBVExpr
toSBVExpr _ (Num e) = pure $ SBVVal $ svInteger KFloat e
toSBVExpr _ (Bool e) = pure $ SBVVal $ svBool e
toSBVExpr svals (Fun argName t e) = do
  a <- svNewVar (toKind t) argName
  toSBVExpr (insert argName a svals) e
toSBVExpr svals (App f' x') = do
  (SBVFun f) <- toSBVExpr svals f'
  (SBVVal x) <- toSBVExpr svals x'
  return $ f x
toSBVExpr svals (Var "+") = pure $ sbvBin svPlus
toSBVExpr svals (Var ">") = pure $ sbvBin svGreaterThan
toSBVExpr svals (Var ">=") = pure $ sbvBin svGreaterEq
toSBVExpr svals (Var name) = pure $ SBVVal $ svals ! name

toKind :: Type -> Kind
toKind TNum = KFloat
toKind TBool = KBool

toSVal :: SBVExpr -> Symbolic SVal
toSVal (SBVVal a) = pure a

-- a = satWithAll [z3] $ do
--   sbvExpr <- toSBVExpr empty $ Fun "a" TNum (App (App (Var ">=") (Var "a")) (Var "a"))
--   toSVal sbvExpr

module Language.Nabla.TypeChecker where
import Prelude hiding (lookup)
import System.IO.Unsafe
import Data.SBV (symbolic)
import Data.SBV.Dynamic
import Control.Monad.State
import Data.Map (empty, fromList, insert, lookup, Map, member, (!))
import Language.Nabla.AST
-- Type Environment
type TypeEnv = Map String Type

data TypeError
  = UnmatchableTypeError Expr Type
  | UnmatchableSieveError Expr Sieve
  | UnapplicableTypeError Type Type
  | NotFoundVarError String
  deriving (Eq)

instance Show TypeError where
  show (UnmatchableTypeError e t) = show e <> " is not " <> show t
  show (UnmatchableSieveError e s) = show e <> " is not " <> show s
  show (UnapplicableTypeError tf t) = "can't apply: (" <> show tf <> ") " <> show t
  show (NotFoundVarError name) = "not found: " ++ name

valid :: TypeEnv -> TypedExpr -> Either TypeError Type
valid env (TypedExpr e Nothing) = infer env e
valid env (TypedExpr e (Just sieve@(Sieve sf@(Fun _ t e')))) = do
  t' <- infer env e
  if t == t'
    then pure t
    else Left $ UnmatchableTypeError e t
  let (SBVFun f) = toSBVExpr empty sf
  let (SBVVal x) = toSBVExpr empty e
  let (SBVVal r) = f x
  let (_, _, ThmResult result) = unsafePerformIO $ proveWithAny [z3] (pure r)
  case result of
    (Unsatisfiable _ _) -> pure t -- Q.E.D.
    _ -> Left $ UnmatchableSieveError e sieve

infer :: TypeEnv -> Expr -> Either TypeError Type
infer _ (Num _) = pure TNum
infer _ (Bool _) = pure TBool
infer env (Var x) = do
  case lookup x env of
    Just t  -> pure t
    Nothing -> Left $ NotFoundVarError x
infer env (Fun parm t e) = TFun t <$> infer (insert parm t env) e
infer env (App f arg) = join $ typeApp <$> infer env f <*> infer env arg

typeApp :: Type -> Type -> Either TypeError Type
typeApp tf@(TFun t r) t'
  | t == t' = pure r
  | otherwise = Left $ UnapplicableTypeError tf  t'

data SBVExpr
  = SBVVal SVal
  | SBVFun (SVal -> SBVExpr)

instance Show SBVExpr where
  show (SBVVal _) = "SBVVal"
  show (SBVFun _) = "SBVFun"

sbvUnary :: (SVal -> SVal) -> SBVExpr
sbvUnary unary = SBVFun $ \a -> SBVVal $ unary a

sbvBin :: (SVal -> SVal -> SVal) -> SBVExpr
sbvBin bin = SBVFun $ \a -> SBVFun $ \b -> SBVVal $ bin a b

toSBVExpr :: Map String SVal -> Expr -> SBVExpr
toSBVExpr _ (Num e) = SBVVal $ svDouble e
toSBVExpr _ (Bool e) = SBVVal $ svBool e
toSBVExpr svals (Fun argName t e) = SBVFun $ \a -> toSBVExpr (insert argName a svals) e
toSBVExpr svals (App f' x') = do
  let (SBVFun f) = toSBVExpr svals f'
  let (SBVVal x) = toSBVExpr svals x'
  f x
toSBVExpr svals (Var name)
  | member name fixtureSBVExpr = fixtureSBVExpr ! name
  | otherwise = SBVVal $ svals ! name

toKind :: Type -> Kind
toKind TNum = KFloat
toKind TBool = KBool

toSVal :: SBVExpr -> SVal
toSVal (SBVVal a) = a

fixtureSBVExpr :: Map String SBVExpr
fixtureSBVExpr = fromList
  [ ("+", sbvBin svPlus)
  , ("-", sbvBin svMinus)
  , ("*", sbvBin svTimes)
  , ("/", sbvBin svDivide)
  , (">", sbvBin svGreaterThan)
  , (">=", sbvBin svGreaterEq)
  , ("<", sbvBin svLessThan)
  , ("<=", sbvBin svLessEq)
  , ("&&", sbvBin svAnd)
  , ("||", sbvBin svOr)
  , ("[unary]-", sbvUnary svUNeg)
  , ("[unary]+", sbvUnary svAbs)
  , ("[unary]!", sbvUnary svNot)
  ]

a = proveWithAny [z3] $ do
  let sbvExpr = toSBVExpr empty $ App (Fun "a" TNum (App (App (Var ">=") (Var "a")) (Var "a"))) (Num 1)
  pure $ toSVal sbvExpr

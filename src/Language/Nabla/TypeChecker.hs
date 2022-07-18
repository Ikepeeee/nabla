{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Nabla.TypeChecker where
import Prelude hiding (lookup, filter)
import System.IO.Unsafe
import Data.SBV (symbolic)
import Data.SBV.Dynamic
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Identity
import Data.Map (empty, fromList, insert, delete, lookup, Map, member, (!), filter, toList, findWithDefault, elems)
import Language.Nabla.AST
import Debug.Trace

-- Type Environment
type TypeEnv = Map String Type

data TypeError
  = UnmatchableTypeError Expr Type
  | UnmatchableSieveError Expr Sieve
  | UnapplicableTypeError Type Type
  | NotFoundVarError String
  | NotFoundTypeVarError Int
  | CannotUnify Type Type
  deriving (Eq)

newtype Infer a = Infer (StateT TypeEnv (Except TypeError) a)
　deriving (Functor, Applicative, Monad, MonadState TypeEnv, MonadError TypeError)

instance Show TypeError where
  show (UnmatchableTypeError e t) = show e <> " is not " <> show t
  show (UnmatchableSieveError e s) = show e <> " is not " <> show s
  show (UnapplicableTypeError tf t) = "can't apply: (" <> show tf <> ") " <> show t
  show (NotFoundTypeVarError id) = "T" <> show id <> " is not found"
  show (NotFoundVarError name) = "not found: " ++ name
  show (CannotUnify t1 t2) = ""

evalInfer :: TypeEnv -> Infer a -> Either TypeError a
evalInfer env (Infer f) = runExcept $ evalStateT f env

runInfer :: TypeEnv -> Infer a -> Either TypeError (a, TypeEnv)
runInfer env (Infer f) = runExcept $ runStateT f env

valid :: TypeEnv -> TypedExpr -> Either TypeError Type
valid env e = evalInfer env (valid' e)

valid' :: TypedExpr -> Infer Type
valid' (TypedExpr e Nothing) = infer' e
valid' (TypedExpr e (Just sieve@(Sieve t sf@(Fun arg e')))) = do
  modify $ insert arg t
  sieveType <- infer' sf -- sieve is t -> Bool
  if sieveType /= TFun t TBool
    then throwError $ UnmatchableTypeError sf (TFun t TBool)
    else pure ()
  t' <- infer' e
  if t == t'
    then pure t
    else throwError $ UnmatchableTypeError e t
  let (SBVFun f) = toSBVExpr empty sf
  let x = toSBVExpr empty e
  let (SBVVal r) = f x
  let (_, _, ThmResult result) = unsafePerformIO $ proveWithAny [z3] (pure r)
  case result of
    (Unsatisfiable _ _) -> pure t -- Q.E.D.
    _ -> throwError $ UnmatchableSieveError e sieve

infer :: TypeEnv -> Expr -> Either TypeError Type
infer env e = evalInfer env (infer' e)

infer' :: Expr -> Infer Type
infer' (Num _) = pure TNum
infer' (Bool _) = pure TBool
infer' (Var x) = do
  env <- get
  case lookup x env of
    Just t  -> pure t
    Nothing -> throwError $ NotFoundVarError x
infer' (Fun param e) = do
  env <- get
  var <- createTVar
  -- let paramType = findWithDefault var param env
  (retType, env') <- case runInfer (insert param var env) (infer' e) of
    Right t -> pure t
    Left e -> throwError e
  -- modify $ delete parm -- delete local scope param
  modify $ insert param (env' ! param)
  pure $ TFun (env' ! param) retType
infer' (App f arg) = do
  ft <- infer' f
  at <- infer' arg
  rt <- typeApp ft at
  unify ft (TFun at rt)
  pure rt

unify :: Type -> Type -> Infer ()
unify (TFun p1 e1) (TFun p2 e2) = do
  unify p1 p2
unify t1@(TVar i1) t2@(TVar i2)
  | i1 == i2  = return ()
unify (TVar i1) t2 = unifyVar i1 t2
unify t1 (TVar i2) = unifyVar i2 t1
unify t1 t2
  | t1 == t2  = return ()
  | otherwise = throwError $ CannotUnify t1 t2

unifyVar :: Int -> Type -> Infer ()
unifyVar id t = do
  env <- get
  let types = toList $ filter (byID id) env
  case types of
    [(name, _)] -> do
      map <- get
      modify $ insert name t
    _ -> throwError $ NotFoundTypeVarError id

isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _ = False

byID :: Int -> Type -> Bool
byID id (TVar id') = id == id'
byID _ _ = False

typeApp :: Type -> Type -> Infer Type
typeApp (TFun paramType retType) argType
  | paramType == argType = pure retType
  | isTVar argType = pure retType
  | isTVar paramType = pure $ appInstance paramType argType retType
  | otherwise = throwError $ UnapplicableTypeError paramType argType
typeApp paramType argType = throwError $ UnapplicableTypeError paramType argType

-- Replace a type variable to instance type
-- appInstance T1 Num (T1 -> T2) == Num -> T2
appInstance :: Type -> Type -> Type -> Type
appInstance tgtType insType (TFun paramType retType)
  = TFun (appInstance tgtType insType paramType) (appInstance tgtType insType retType)
appInstance tgtType insType retType
  | retType == tgtType = insType
  | otherwise = retType
data SBVExpr
  = SBVVal SVal
  | SBVFun (SBVExpr -> SBVExpr)

instance Show SBVExpr where
  show (SBVVal _) = "SBVVal"
  show (SBVFun _) = "SBVFun"

sbvUnary :: (SVal -> SVal) -> SBVExpr
sbvUnary unary = SBVFun $ \(SBVVal a) -> SBVVal $ unary a

sbvBin :: (SVal -> SVal -> SVal) -> SBVExpr
sbvBin bin = SBVFun $ \(SBVVal a) -> SBVFun $ \(SBVVal b) -> SBVVal $ bin a b

toSBVExpr :: Map String SBVExpr -> Expr -> SBVExpr
toSBVExpr _ (Num e) = SBVVal $ svDouble e
toSBVExpr _ (Bool e) = SBVVal $ svBool e
toSBVExpr svals (Fun argName e) = SBVFun $ \a -> toSBVExpr (insert argName a svals) e
toSBVExpr svals (App f' x') = do
  let (SBVFun f) = toSBVExpr svals f'
  let x = toSBVExpr svals x'
  f x
toSBVExpr svals (Var name)
  | member name fixtureSBVExpr = fixtureSBVExpr ! name
  | otherwise = svals ! name

toSVal :: SBVExpr -> SVal
toSVal (SBVVal a) = a

createTVar :: Infer Type
createTVar = do
  env <- get
  let ns = map (\(TVar n) -> n) $ elems $ filter isTVar env
  pure $ TVar $ maximum (0:ns) + 1

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
  , ("==", sbvBin svEqual)
  , ("&&", sbvBin svAnd)
  , ("||", sbvBin svOr)
  , ("[unary]-", sbvUnary svUNeg)
  , ("[unary]+", sbvUnary svAbs)
  , ("[unary]!", sbvUnary svNot)
  ]

a = proveWith z3 $ do
  let sbvExpr = toSBVExpr empty $ App (Fun "a" (App (App (Var ">=") (Var "a")) (Var "a"))) (Num 1)
  pure $ toSVal sbvExpr

trace' :: Show a => String -> a -> a
trace' m a = trace (m <> show a) a

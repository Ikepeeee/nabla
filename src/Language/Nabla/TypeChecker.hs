{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Nabla.TypeChecker where

import Language.Nabla.AST
import Language.Nabla.Fixture
import Data.SBV
import Control.Concurrent.Async
import Control.Lens (makeLenses, (^.), (.~), (?~), (&))
import Control.Lens.Operators ((<|))

data ProgDict p
  = ProgDict
  { _types :: [(Identifier p, TypeDef p)]
  , _fns :: [(Identifier p, FnDef p)]
  }
data FnDef p
  = FnDef
  { _fn :: Maybe (Fn p)
  , _fnType :: Maybe (FnType p)
  , _fnInferredType :: Maybe (FnType p)
  }

makeLenses ''ProgDict
makeLenses ''FnDef

newProgDict :: ProgDict p
newProgDict = ProgDict [] []

addFn :: Identifier p -> Fn p -> ProgDict p -> ProgDict p
addFn name f dict = case lookup name (dict ^. fns) of
  Nothing -> dict & fns .~ (name, FnDef (Just f) Nothing Nothing):(dict ^. fns)
  Just fnDef -> dict & fns .~ ((name, fnDef & fn ?~ f):delete name (dict ^. fns))

addFnType :: Identifier p -> FnType p -> ProgDict p -> ProgDict p
addFnType name sig dict = case lookup name (dict ^. fns) of
  Nothing -> dict & fns .~ (name, FnDef Nothing (Just sig) Nothing):(dict ^. fns)
  Just fnDef -> dict & fns .~ ((name, fnDef & fnType ?~ sig):delete name (dict ^. fns))

addTypeDef :: Identifier p -> TypeDef p -> ProgDict p -> ProgDict p
addTypeDef name tp dict = case lookup name (dict ^. types) of
  Nothing -> dict & types .~ (name, tp):(dict ^. types)
  Just _ -> undefined -- duplicate error

delete :: Eq k => k -> [(k, a)] -> [(k, a)]
delete k = filter (not . (==) k . fst)

mkProgDict :: Prog p -> ProgDict p -> ProgDict p
mkProgDict (Prog []) dict = dict
mkProgDict (Prog ((NamedUnit (name, UnitFn fn)):us)) dict
  = mkProgDict (Prog us) $ addFn name fn dict
mkProgDict (Prog ((NamedUnit (name, UnitFnType fnType)):us)) dict
  = mkProgDict (Prog us) $ addFnType name fnType dict
mkProgDict (Prog ((NamedUnit (name, UnitTypeDef tp)):us)) dict
  = mkProgDict (Prog us) $ addTypeDef name tp dict

class TypeInference a where
  infer :: a p -> ProgDict p -> [Type p]

class TypeCheck a where
  valid :: a p -> ProgDict p -> [TypeValidationError p]

data TypeValidationError p
  = NameNotFound (Identifier p)
  | ImplementationNotFound (Identifier p)

check :: Prog p -> [TypeValidationError p]
check (Prog us) = valid (Prog us) $
  mkProgDict
  (Prog (us <> map (\(name, t) -> NamedUnit (name, UnitFnType t)) fixtureFnTypes))
  newProgDict

getPos :: TypeValidationError p -> p
getPos (NameNotFound (Identifier p _)) = p

instance Show (TypeValidationError p) where
  show (NameNotFound name) = "name '" <> show name <> "' is not defined"
  show (ImplementationNotFound name) = "the type signature for '" <> show name <> "' is defined, but an accompanying function is not implemented"

instance TypeCheck Prog where
  valid (Prog (u:us)) ts = valid u ts <> valid (Prog us) ts
  valid (Prog []) _ = []

instance TypeCheck NamedUnit where
  valid (NamedUnit (name, UnitFn e)) us = valid e us
  valid (NamedUnit (name, UnitFnType e)) us = []
  valid (NamedUnit (name, UnitTypeDef e)) us = []

instance TypeCheck Fn where
  valid (Fn args body) dict = valid body (foldl (\d (name, f) -> addFn name f d) dict (map toFn args))
    where
      toFn name = (name, Fn [] undefined)

instance TypeCheck Expr where
  valid (Expr _ v []) us = valid v us
  valid (Expr p v vs) us = valid v us <> valid (Expr p (head vs) (tail vs)) us

instance TypeCheck Value where
  valid (Alias name) dict = case findByName name (dict ^. fns) of
    Right _ -> []
    Left e -> [e]
  valid (FnValue fn) us = valid fn us
  valid (Const _ _ ) _ = []
  valid (ExprValue expr) us = valid expr us

instance TypeInference Value where
  infer (Alias name) dict = undefined

findByName :: Identifier p -> [(Identifier p, a)] -> Either (TypeValidationError p) a
findByName name us = case lookup name us of
  Just a -> Right a
  Nothing -> Left $ NameNotFound name

-- isTypeOf :: Expr p -> TypeDef p -> [NamedUnit p] -> Bool
-- isTypeOf e t us = (elementType t) e

-- instance TypeCheck (Expr p) where
--   valid (Expr p v vs) types =

-- instance TypeCheck (Value p) where
--   valid (Alias )

--f :: Integer -> Integer -> Integer
f a b = (a / b) + (a `div` b)

-- a = Expr $ Alias []


-- checkFnDif :: [(Identifier, FnDef)] -> FnDef -> IO (Either (String, m) ())
-- checkFnDif fns (FnDef args' fnBody')
--   = undefined

-- fnDifValidationExpr ::
--   [(Identifier, (FnDif, FnTypeDef))]
--   -> (FnDif, FnTypeDef)
--   -> Symbolic SBool

-- fnDifValidationExpr fns ((FnDif args' fnBody'), (FnTypeDef argTypes' retType')) = do
--   argSimbols <- mapM sInteger args'
--   argConditon <- sAnd <$> mapM (\(t, s) -> (toSBool t s)) (zip argTypes' argSimbols)
--   bodyExpr <- expr (zip args' argSimbols) fnBody'
--   retCondition <- toSBool retType' bodyExpr
--   return $ argConditon .=> retCondition

-- expr :: [(Identifier, SInteger)] -> Fn -> Symbolic SInteger
-- expr args (Const (Direct n)) = pure $ fromIntegral n
-- expr args (Direct name) = case lookup name args of
--   Nothing -> sInteger name
--   Just v -> pure $ v
-- expr args (FnApp "+" [a, b]) = (+) <$> (expr args a) <*> (expr args b)
-- expr args (FnApp "-" [a, b]) = (-) <$> (expr args a) <*> (expr args b)
-- expr args (FnApp "*" [a, b]) = (*) <$> (expr args a) <*> (expr args b)
-- expr args (FnApp "/" [a, b]) = (sDiv) <$> (expr args a) <*> (expr args b)

-- toSBool :: Type -> SBV a -> Symbolic SBool
-- toSBool (TInteger sieve) v = sieveToSBool sieve v
-- toSBool (TDouble sieve) v = sieveToSBool sieve v

-- class SBoolable a b where
--   sieveToSBool :: a -> b -> Symbolic SBool

-- instance (SymVal a, Ord a) => SBoolable (Sieve a) (SBV a) where
--   sieveToSBool None v = pure sTrue
--   sieveToSBool (And a b) v = (.&&) <$> (sieveToSBool a v) <*> (sieveToSBool b v)
--   sieveToSBool (Or a b) v = (.||) <$> (sieveToSBool a v) <*> (sieveToSBool b v)
--   sieveToSBool (Not a) v = sNot <$> (sieveToSBool a v)
--   sieveToSBool (Gt c) v = pure $ v .> literal c
--   sieveToSBool (Lt c) v = pure $ v .< literal c
--   sieveToSBool (Ge c) v = pure $ v .>= literal c
--   sieveToSBool (Le c) v = pure $ v .<= literal c


fibo :: (Mergeable a, EqSymbolic a, Num a) => a -> a
fibo n
  = ite (n .== 1) 1
  $ ite (n .== 2) 1
  $ fibo (n - 1) + fibo (n - 2)

fibo' :: Integer -> Integer
fibo' n
  | n == 1 = 1
  | n == 2 = 1
  | otherwise = fibo' (n - 1) + fibo' (n - 2)

a = prove $ \n -> ((n :: SInteger) .> 0) .=> (fibo n) .> 0

q :: Symbolic SBool
q = do
  x <- sInteger "x"
  y <- sInteger "y"
  return $ x * x + 4 * x - y * y + 2 * y - 2 .== 0

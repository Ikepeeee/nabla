{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Language.Nabla.TypeChecker where

import Language.Nabla.AST
import Data.SBV
import Control.Concurrent.Async
import Text.Megaparsec.Error

class TypeInference a where
  infer :: a -> [Type p]

class TypeCheck a where
  valid :: a p -> [NamedFnType p] -> Either (TypeValidationError p) ()

data TypeValidationError p = NameNotFound (Identifier p) deriving (Eq)

getPos :: TypeValidationError p -> p
getPos (NameNotFound (Identifier p _)) = p

instance Show (TypeValidationError p) where
  show (NameNotFound name) = "name '" <> show name <> "' is not defined"

instance Ord (TypeValidationError p) where
  compare _ _ = EQ

instance ShowErrorComponent (TypeValidationError p) where
  showErrorComponent = show

instance TypeCheck Prog where
  valid (Prog (u:us)) ts = valid u ts *> valid (Prog us) ts
  valid (Prog []) _ = Right ()

instance TypeCheck Unit where
  valid (UnitFnDef e) = valid e
  valid (UnitFnType e) = valid e
  valid (UnitTypeDef e) = valid e

instance TypeCheck NamedFnDef where
  valid (NamedFnDef name _) _ = Left $ NameNotFound name

instance TypeCheck NamedFnType where
  valid (NamedFnType name _) _ = Left $ NameNotFound name

instance TypeCheck NamedTypeDef where
  valid (NamedTypeDef name _) _ = Left $ NameNotFound name

findByName :: Identifier p -> [(Identifier p, a)] -> Either (TypeValidationError p) a
findByName name types = case lookup name types of
  Just a -> Right a
  Nothing -> Left $ NameNotFound name

-- instance TypeCheck (Expr p) where
--   valid (Expr p v vs) types =

-- instance TypeCheck (Value p) where
--   valid (Alias )

--f :: Integer -> Integer -> Integer
f a b = (a / b) + (a `div` b)


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

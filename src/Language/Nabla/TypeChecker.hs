{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Nabla.TypeChecker where

import Data.List (find)
import qualified Data.Map (Map)
import Data.Maybe (fromJust, isNothing)
import Language.Nabla.AST
import Data.SBV
import Control.Concurrent.Async

class TypeInference a where
  infer :: a -> [Type p]

class TypeCheck a where
  valid ::  a -> [(Identifier p, FnType p)] -> Either (String, p) ()

findByName :: Identifier p -> [(Identifier p, a)] -> Either (String, p) a
findByName (Identifier p name) types = case lookup (Identifier p name) types of
  Just a -> Right a
  Nothing -> Left ("name '" <> name <> "' is not defined", p)

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

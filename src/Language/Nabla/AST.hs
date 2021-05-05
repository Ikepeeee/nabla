module Language.Nabla.AST where

import Data.List (intercalate)
import Data.UUID (UUID)

data TypedExpr
  = TypedExpr Expr (Maybe Sieve)
  deriving (Eq, Show)

data Type
  = TFun Type Type
  | TNum
  | TBool
  | TVar UUID
  deriving (Eq)

-- base type
-- condition
-- ex) { n : TNum | n > 0 }
data Sieve = Sieve Type Expr deriving (Show, Eq)

instance Show Type where
  show TNum = "Num"
  show TBool = "Bool"
  show (TFun p e) = pp p ++ " -> " ++ show e
    where pp fun@(TFun _ _) = "(" ++ show fun ++ ")"
          pp t = show t
  show (TVar id) = "Var-" <> show id

data Expr
  = Num Double
  | Bool Bool
  | Var String
  | Fun String Expr
  | App Expr Expr
  deriving (Eq, Show)

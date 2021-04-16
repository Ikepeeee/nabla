module Language.Nabla.AST where

import Data.List (intercalate)

data TypedExpr
  = TypedExpr Expr (Maybe Type)
  deriving (Eq, Show)

data Type
  = TFun Type Type
  | TNum
  | TBool
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

data Expr
  = Num Integer
  | Bool Bool
  | Var String
  | Fun String Type Expr
  | App Expr Expr
  deriving (Eq, Show)

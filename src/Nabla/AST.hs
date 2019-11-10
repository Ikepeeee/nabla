{-# LANGUAGE FlexibleInstances #-}

module Nabla.AST where

import Data.List (intercalate)

newtype AST = AST [Expr] deriving (Eq)

instance Show AST where
  show (AST exprs) = intercalate "\n" $ map show exprs

data Expr
  = ValueExpr Value
  | VariableExpr Identifier
  | Assign Identifier Expr
  | Complex (ComplexValue Expr)
  deriving (Eq)

type Identifier = String

instance Show Expr where
  show (ValueExpr v) = show v
  show (VariableExpr name) = name
  show (Assign name v) = name <> " = " <> show v
  show (Complex c) = show c

data Value
  = SimpleV SimpleValue
  | ComplexV (ComplexValue Value)
  deriving (Eq)

instance Show Value where
  show (SimpleV v) = show v
  show (ComplexV v) = show v

data SimpleValue
  = NumberV String
  | StringV String
  | SymbolV String
  deriving (Eq)

type Context = String
data ComplexValue v = WrapValues Context [v] deriving (Eq)

instance Show (ComplexValue Value) where
  show (WrapValues s vs) = s <> " " <> vs'
    where
      vs' = unwords $ map showChild vs
      showChild (SimpleV v) = show v
      showChild (ComplexV (WrapValues context [])) = context
      showChild v = "(" <> show v <> ")"

instance Show (ComplexValue Expr) where
  show (WrapValues s vs) = s <> " " <> vs'
    where
      vs' = unwords $ map showChild vs
      showChild v = "(" <> show v <> ")"

instance Show SimpleValue where
  show (NumberV v) = v
  show (StringV v) = "'" <> v <> "'"
  show (SymbolV v) = ":" <> v

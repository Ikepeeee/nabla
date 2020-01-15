{-# LANGUAGE FlexibleInstances #-}

module Nabla.AST where

import Data.List (intercalate)

data AST = AST [Unit] deriving (Eq)

instance Show AST where
  show (AST units) = intercalate "\n" $ map show units

data Unit
  = ExprUnit Expr
  | StatUnit Stat
  deriving (Eq)

instance Show Unit where
  show (ExprUnit e) = show e
  show (StatUnit s) = show s

data Stat = TypeAssign Identifier TypeExpr deriving (Eq)

instance Show Stat where
  show (TypeAssign name t) = name <> " :: " <> (show t)

data Expr
  = ValueExpr Value
  | VariableExpr Identifier
  | Assign Identifier Expr
  deriving (Eq)

type Identifier = String

data TypeExpr
  = TypeName Identifier
  deriving (Eq)

instance Show TypeExpr where
  show (TypeName name) = name

instance Show Expr where
  show (ValueExpr v) = show v
  show (VariableExpr name) = name
  show (Assign name v) = name <> " = " <> show v

data Value
  = SimpleV SimpleValue
  | ComplexV ComplexValue
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
data ComplexValue = WrapValues Context [Expr] deriving (Eq)

instance Show ComplexValue where
  show (WrapValues s vs) = s <> " " <> vs'
    where
      vs' = unwords $ map showChild vs
      showChild v = "(" <> show v <> ")"

instance Show SimpleValue where
  show (NumberV v) = v
  show (StringV v) = "'" <> v <> "'"
  show (SymbolV v) = ":" <> v

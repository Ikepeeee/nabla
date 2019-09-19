module Nabla.AST where

import Data.List (intercalate)

newtype AST = AST [Expr] deriving (Eq)

instance Show AST where
  show (AST exprs) = intercalate "\n" $ map show exprs

data Expr = ValueExpr Value deriving (Eq)

instance Show Expr where
  show (ValueExpr v) = show v

data Value = SimpleV SimpleValue deriving (Eq)

instance Show Value where
  show (SimpleV v) = show v

data SimpleValue
  = NumberV String
  | StringV String
  | SymbolV String
  deriving (Eq)

instance Show SimpleValue where
  show (NumberV v) = v
  show (StringV v) = "'" <> v <> "'"
  show (SymbolV v) = ":" <> v

{-#  LANGUAGE FlexibleInstances #-}


module Language.Nabla.AST where

import Data.List (intercalate)

type AliasName = String

data Type p = Type (Identifier p)

data TypeDef p = TypeDef
  { element :: Identifier p
  , elementType :: Type p
  , sieve :: Expr p
  }

instance Show (TypeDef p) where
  show td = show (element td) <> " :: " <> show (elementType td) <> " | " <> show (sieve td)

instance Show (Type p) where
  show (Type name) = show name

data Prog p = Prog [NamedUnit p]

instance Show (Prog p) where
  show (Prog ps) = intercalate "\n" $ map show ps

newtype NamedUnit p = NamedUnit (Identifier p, Unit　p)
data Unit　p
  = UnitFn (Fn p)
  | UnitFnType (FnType p)
  | UnitTypeDef (TypeDef p)

instance Show (NamedUnit p) where
  show (NamedUnit (name, UnitFn u)) = show name <> " = " <> show u
  show (NamedUnit (name, UnitFnType u)) = show name <> " :: " <> show u
  show (NamedUnit (name, UnitTypeDef u)) = show name <> " = " <> show u

data Fn p
  = Fn
    { fnArgs :: [(Identifier p)]
    , fnBody :: (Expr p)
    }

instance Eq (Fn p) where
  (Fn args1 body1) == (Fn args2 body2) = args1 == args2 && body1 == body2

instance Show (Fn p) where
  show (Fn args fnBody) =
    "\\"
    <> (intercalate " " $ map show args)
    <> " -> "
    <> (show fnBody)

data FnType p
  = FnType
    { argTypes :: [Type p]
    , retType :: Type p
    }

instance Show (FnType p) where
  show (FnType args ret) = intercalate " -> " $ map show (args <> [ret])

data Expr p = Expr p (Value p) [Value p]

instance Eq (Expr p) where
  (Expr _ f1 args1) == (Expr _ f2 args2) = f1 == f2 && args1 == args2

instance Show (Expr p) where
  show (Expr _ v vs) = (intercalate " " $ map show (v:vs))

data Value p
  = Alias (Identifier p)
  | FnValue (Fn p)
  | Const p Const
  | ExprValue (Expr p)

instance Show (Value p) where
  show (Alias s) = show s
  show (FnValue f) = "(" <> show f <> ")"
  show (Const _ c) = show c
  show (ExprValue e) = "(" <> show e <> ")"

instance Eq (Value p) where
  (Alias a) == (Alias b) = a == b
  (FnValue a) == (FnValue b) = a == b
  (Const _ a) == (Const _ b) = a == b
  (ExprValue a) == (ExprValue b) = a == b

data Const
  = Direct String
  | Indirect String
  deriving (Eq)

instance Show Const where
  show (Direct c) = c
  show (Indirect c) = "\"" <> c <> "\""

data Identifier p
  = Identifier p String
  | Fixture String

instance Show (Identifier p) where
  show (Identifier _ s) = s
  show (Fixture s) = s

instance Eq (Identifier p) where
  (Identifier _ a) == (Identifier _ b) = a == b
  (Fixture a) == (Fixture b) = a == b
  (Identifier _ a) == (Fixture b) = a == b
  (Fixture a) == (Identifier _ b) = a == b

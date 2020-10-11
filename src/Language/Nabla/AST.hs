{-#  LANGUAGE FlexibleInstances #-}


module Language.Nabla.AST where

import Data.List (intercalate)

type AliasName = String

data Type p = Type (Identifier p)

data NamedTypeDef p = NamedTypeDef (Identifier p) (TypeDef p)

instance Show (NamedTypeDef p) where
  show (NamedTypeDef name typedef) = show name <> " = " <> "{ " <> show typedef <>  " }"

data TypeDef p = TypeDef
  { element :: Identifier p
  , elementType :: Type p
  , sieve :: Expr p
  }

instance Show (TypeDef p) where
  show td = show (element td) <> " :: " <> show (elementType td) <> " | " <> show (sieve td)

instance Show (Type p) where
  show (Type name) = show name

data Prog p = Prog [Unit p]

instance Show (Prog p) where
  show (Prog ps) = intercalate "\n" $ map show ps

data Unit　p
  = UnitFnDef (NamedFnDef p)
  | UnitFnType (NamedFnType p)
  | UnitTypeDef (NamedTypeDef p)

instance Show (Unit p) where
  show (UnitFnDef u) = show u
  show (UnitFnType u) = show u
  show (UnitTypeDef u) = show u

data NamedFnDef p = NamedFnDef (Identifier p) (Fn p)

instance Show (NamedFnDef p) where
  show (NamedFnDef n f) = show n <> " = " <> show f

data Fn p
  = Fn
    { args :: [(Identifier p)]
    , fnBody :: (Expr p)
    }

instance Show (Fn p) where
  show (Fn args fnBody) =
    "\\"
    <> (intercalate " " $ map show args)
    <> " -> "
    <> (show fnBody)

data NamedFnType p = NamedFnType (Identifier p) (FnType p)

instance Show (NamedFnType p) where
  show (NamedFnType name fnType) = show name <> " :: " <> show fnType

data FnType p
  = FnType
    { argTypes :: [Type p]
    , retType :: Type p
    }

instance Show (FnType p) where
  show (FnType args ret) = intercalate " -> " $ map show (args <> [ret])

data Expr p = Expr p (Value p) [Value p]

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
  _ == _ = False

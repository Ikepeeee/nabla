module Language.Nabla.AST where

type TypeName = String

data NFunc = NFunc [NArg] NValue String TypeName NValue deriving (Show)

data NArg = NArg String TypeName NValue   deriving (Show)

data NValue
  = NDouble Double
  | NBool Bool
  | NString String
  | NRegex String
  | NVar String
  | NIte NValue NValue NValue
  | NBin String NValue NValue
  | NUni String NValue
  | NApp String [NValue]
  deriving (Show)

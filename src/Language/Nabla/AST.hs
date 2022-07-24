module Language.Nabla.AST where

type TypeName = String

data NFunc = NFunc [NArg] NValue String TypeName NValue deriving (Show)

data NArg = NArg String TypeName NValue deriving (Show)

argName :: NArg -> String
argName (NArg name _ _) = name
data NValue
  = NDouble Double
  | NBool Bool
  | NString String
  | NRegex String
  | NVar String
  | NIte NValue NValue NValue
  | NFixtureApp String [NValue]
  | NApp String [NValue]
  deriving (Show)

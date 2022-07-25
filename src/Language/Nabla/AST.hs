module Language.Nabla.AST where

type TypeName = String

data NFun = NFun [NArg] NValue NSieve deriving (Show)

data NSieve = NSieve String TypeName NValue deriving (Show)

data NArg = NArg String NSieve deriving (Show)

argName :: NArg -> String
argName (NArg name _) = name
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

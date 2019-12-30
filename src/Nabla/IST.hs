module Nabla.IST where

type Identifier = String
type Var = (Identifier, Value)
type TypedVar = (Identifier, [Type])
type TypeVar = (Identifier, Type)

type TypedValue = (Value, [Type])

data Value
  = NumberV String
  | StringV String
  | SymbolV String
  | ComplexV Context [Value]

instance Show Value where
  show (NumberV v) = v
  show (StringV v) = "'" <> v <> "'"
  show (SymbolV v) = ":" <> v
  show (ComplexV c vs) = c <> " " <> unwords (map show vs)

type Context = String

type Type = Value -> Bool

infer :: Value -> [Type] -> [Type]
infer v ts = filter (\t -> t v) ts

module Nabla.IST where

type Identifier = String
type Var = (Identifier, Value)
type Signature = (Identifier, [Type])
data Type = Type
  { typeName :: Identifier
  , typeSieve :: (Value -> Bool)
  }

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

infer :: Value -> [Type] -> [Type]
infer v = filter (\(Type _ t) -> t v)

module Nabla.Parser2 where

import Control.Monad.IO.Class
import Data.Void
import Control.Monad.State
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (intercalate)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

prog :: Parser Value
prog = eval <$> expr <* eof

data Expr = Expr [Value]
  deriving (Show)

expr :: Parser Expr
expr = Expr <$> many value

data Value
  = ConstV Const
  | LambdaV Lambda
  deriving (Show)

value :: Parser Value
value = constant <|> lambda

data Lambda = Lambda (Value -> Value)

lambda :: Parser Value
lambda = choice
  [ pure add <* L.lexeme sc (string "add")
  , pure inc <* L.lexeme sc (string "inc")
  ]

add :: Value
add = LambdaV $ Lambda $ \(ConstV (IntV a))
  -> LambdaV $ Lambda $ \(ConstV (IntV b))
  -> ConstV $ IntV (a + b)

data Type = Type
  { typeName :: Identifier
  , typeSieve :: (Value -> Bool)
  }

instance Show Type where
  show = typeName

int :: Type
int = Type { typeName = "int", typeSieve = int' }

inc :: Value
inc = LambdaV $ Lambda $ \(ConstV (IntV a))
  -> ConstV $ IntV (a + 1)

int' :: Value -> Bool
int' (ConstV (IntV _)) = True
int' _ = False

instance Show Lambda where
  show _ = "lambda"

data Const
  = IntV Integer
  | FloatV Double
  | StringV String
  | SymbolV String
  | ComplexV Context [Value]
  deriving (Show)

constant :: Parser Value
constant
  = ConstV . IntV <$> intT
  <|> ConstV . FloatV <$> floatT
  <|> ConstV . StringV <$> stringT
  <|> ConstV . SymbolV <$> symbolT
  <|> ConstV <$> (ComplexV <$> contextT <*> many value)

intT :: Parser Integer
intT = L.signed sc $ L.lexeme sc L.decimal

floatT :: Parser Double
floatT = L.signed sc $ L.lexeme sc L.float

stringT :: Parser String
stringT = L.lexeme sc $ (str '\'') <|> (str '"') <|> (str '`')
  where
    str :: Char -> Parser String
    str c = char c *> manyTill L.charLiteral (char c)

symbolT :: Parser String
symbolT = L.lexeme sc (char ':' *> some alphaNumChar)

contextT :: Parser String
contextT = L.lexeme sc $ (:) <$> upperChar <*> many alphaNumChar

eval :: Expr -> Value
eval (Expr (v:[])) = v
eval (Expr ((LambdaV (Lambda f):v:vs))) = eval (Expr ((f v):vs))
eval (Expr (v1:v2:vs)) = eval (Expr (v2:vs))

type Identifier = String
type Context = String

identifierT :: Parser String
identifierT = L.lexeme sc $ (:) <$> lowerChar <*> many alphaNumChar

sc :: Parser ()
sc = L.space
  (skipSome (char ' ' <|> char '\t'))
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

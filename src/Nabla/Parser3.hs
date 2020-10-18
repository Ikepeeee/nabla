{-# LANGUAGE FlexibleInstances #-}
module Nabla.Parser3 where

import Control.Monad.IO.Class
import Data.Void
import Control.Monad.State
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (intercalate)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data NodeMeta = NodeMeta SourcePos

data Type = Type
  { typeName :: Identifier
  , typeSieve :: (Value -> Bool)
  }

instance Show Type where
  show (Type name _) = name
data Data　= Data { type_ :: Type, value_ :: Value }

data AtomValue
  = Direct String
  | Indirect String
  deriving (Show, Eq)
type Identifier = String

type MetaValue = Value

data Value
  = Atom AtomValue
  | Meta MetaValue (Maybe Value)
  | List [Value]
  | Tuple [Value]
  | Map [(Value, Value)]
  deriving (Show, Eq)

type' :: Parser Type
type' = Type <$> identifierT' <*> (assignT >> setL *> (toType <$> many value) <* setR)
  where
    setL = L.lexeme sc $ string "{"
    setR = L.lexeme sc $ string "}"
class Sieve a where
  toType :: a -> Value -> Bool

instance Sieve [Value] where
  toType vs v = elem v vs

identifierT :: Parser String
identifierT = L.lexeme sc $ (:) <$> lowerChar <*> many alphaNumChar

identifierT' :: Parser String
identifierT' = L.lexeme sc $ (:) <$> upperChar <*> many alphaNumChar

assignT :: Parser String
assignT = L.lexeme sc $ string "="

colonT :: Parser String
colonT = L.lexeme sc $ string ":"

sharpT :: Parser String
sharpT = string "#"

--value' :: Parser (NodeMeta, Value)
--value' =  NodeMeta <$> getSourcePos <*> value

value :: Parser Value
value = choice
  [listValue
  ,tupleValue
  ,mapValue
  ,metaValue
  ,atomValue
  ,groupL *> value <* groupR
  ]
  where
    groupL = L.lexeme sc $ string "("
    groupR = L.lexeme sc $ string ")"

atomValue :: Parser Value
atomValue = choice [Atom . Direct <$> direct, Atom . Indirect <$> indirect]

metaValue :: Parser Value
metaValue = Meta <$> (sharpT *> value) <*> optional value

listValue :: Parser Value
listValue = List <$> (listL *> (many value) <* listR)
  where
    listL = L.lexeme sc $ string "["
    listR = L.lexeme sc $ string "]"

tupleValue :: Parser Value
tupleValue = Tuple <$> (tupleL *> (many value) <* tupleR)
  where
    tupleL = L.lexeme sc $ string "t("
    tupleR = L.lexeme sc $ string ")"

mapValue :: Parser Value
mapValue = Map <$> (mapL *> (many pair) <* mapR)
  where
    pair = pairL *> ((,) <$> value <*> value) <* pairR
    mapL = L.lexeme sc $ string "{"
    mapR = L.lexeme sc $ string "}"
    pairL = L.lexeme sc $ string "("
    pairR = L.lexeme sc $ string ")"

direct :: Parser String
direct = L.lexeme sc $ (text <|> number)
  where
    text = (:) <$> upperChar <*> many alphaNumChar
    number = (<>) <$> minus <*> positive
    positive = (<>) <$> integer <*> option "" decimal
    integer = some numberChar
    decimal = (<>) <$> dot <*> some numberChar
    minus = option "" (string "-")
    dot = L.lexeme sc $ string "."

indirect :: Parser String
indirect = L.lexeme sc $ (str '\'') <|> (str '"') <|> (str '`')
  where
    str :: Char -> Parser String
    str c = char c *> manyTill L.charLiteral (char c)

sc :: Parser ()
sc = L.space
  (skipSome (char ' ' <|> char '\t'))
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

module Nabla.Parser where

import Control.Monad.IO.Class
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Nabla.AST

type Parser = Parsec Void String

ast :: Parser AST
ast = AST <$> (scn *> many (unit <* some newlineT) <* eof)

unit :: Parser Unit
unit = try (StatUnit <$> stat) <|> (ExprUnit <$> expr)

stat :: Parser Stat
stat = TypeAssign <$> identifierT <*> (typeAssignT *> typeExpr)

expr :: Parser Expr
expr
  = try (Assign <$> identifierT <*> (assignT *> expr))
  <|> try (VariableExpr <$> identifierT)
  <|> try (ValueExpr <$> value)
  <|> try (Function <$> identifierT)
  <|> Apply <$> expr <*> value

typeExpr :: Parser TypeExpr
typeExpr = TypeName <$> identifierT

value :: Parser Value
value
  = SimpleV <$> simpleValue
  <|> ComplexV <$> complexValue
  <|> (leftT *> value <* rightT)

simpleValue :: Parser SimpleValue
simpleValue
  = NumberV <$> numberT
  <|> StringV <$> stringT
  <|> SymbolV <$> symbolT

complexValue :: Parser ComplexValue
complexValue = WrapValues <$> contextT <*> many expr

numberT :: Parser String
numberT = try (show <$> floatT) <|> (show <$> intT)
  where
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

newlineT :: Parser String
newlineT = L.lexeme sc $ (:[]) <$> newline

contextT :: Parser String
contextT = L.lexeme sc $ (:) <$> upperChar <*> many alphaNumChar

leftT :: Parser String
leftT = L.lexeme sc $ string "("

rightT :: Parser String
rightT = L.lexeme sc $ string ")"

assignT :: Parser String
assignT = L.lexeme sc $ string "="

typeAssignT :: Parser String
typeAssignT = L.lexeme sc $ string "::"

identifierT :: Parser String
identifierT = L.lexeme sc $ (:) <$> lowerChar <*> many alphaNumChar

scn :: Parser ()
scn = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

sc :: Parser ()
sc = L.space
  (skipSome (char ' ' <|> char '\t'))
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

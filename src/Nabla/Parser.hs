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
ast = AST <$> (scn *> many (expr <* some newlineT) <* eof)

expr :: Parser Expr
expr = ValueExpr <$> value

value :: Parser Value
value = SimpleV <$> simpleValue

simpleValue :: Parser SimpleValue
simpleValue
  = NumberV <$> numberT
  <|> StringV <$> stringT
  <|> SymbolV <$> symbolT

numberT :: Parser String
numberT = try (show <$> floatT) <|> (show <$> intT)
  where
    intT :: Parser Integer
    intT = L.signed sc $ L.lexeme sc L.decimal
    floatT :: Parser Double
    floatT = L.signed sc $ L.lexeme sc L.float

stringT :: Parser String
stringT = L.lexeme sc (char '\'' *> manyTill L.charLiteral (char '\''))

symbolT :: Parser String
symbolT = L.lexeme sc (char ':' *> some alphaNumChar)

newlineT :: Parser String
newlineT = L.lexeme sc $ (:[]) <$> newline

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

{-# LANGUAGE FlexibleInstances #-}

module Language.Nabla.Parser where

import Control.Monad.Trans (MonadTrans (lift))
import Data.Scientific (toRealFloat)
import Data.Void
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec
import Text.Megaparsec.Char
    ( alphaNumChar, char, lowerChar, space1, string )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Language.Nabla.AST

type Parser = Parsec Void String

pFuns :: Parser [(String, NFun)]
pFuns = sc *> many pFun <* eof

pFun :: Parser (String, NFun)
pFun = do
  name <- tIdent
  lexeme $ char '('
  args <- pArg `sepBy` lexeme (char ',')
  lexeme $ char ')'
  lexeme $ string "->"
  expr <- pExpr
  lexeme $ string ":"
  t <- pSieve
  pure (name, NFun args expr t)

pArg :: Parser NArg
pArg = do
  arg <- tIdent
  lexeme $ string ":"
  NArg arg <$> pSieve

pSieve :: Parser NSieve
pSieve = do
  lexeme $ char '{'
  argName <- tIdent
  lexeme $ char ':'
  t <- lexeme $ string "Double" <|> string "Bool" <|> string "String"
  lexeme $ char '|'
  expr <- pExpr
  lexeme $ char '}'
  pure $ NSieve argName t expr

pExpr :: Parser NValue
pExpr = makeExprParser pTerm operatorTable <?> "expression"

operatorTable :: [[Operator Parser NValue]]
operatorTable =
  [ [ prefix "-" (uni "-")
    , prefix "+" id
    ]
  , [ binary "*" (bin "*")
    , binary "/" (bin "/")
    ]
  , [ binary "+" (bin "+")
    , binary "-" (bin "-")
    ]
  , [ binary ">=" (bin ">=")
    , binary ">" (bin ">")
    , binary "<=" (bin "<=")
    , binary "<" (bin "<")
    , binary "==" (bin "==")
    , binary "!=" (bin "/=")
    , binary "~=" (bin "~=")
    ]
  , [ prefix "!" (uni "!")
    ]
  , [ binary "&&" (bin "&&")
    , binary "||" (bin "||")
    ]
  ]

uni :: String -> NValue -> NValue
uni op a = NFixtureApp op [a]

bin :: String -> NValue -> NValue -> NValue
bin op a b = NFixtureApp op [a, b]

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

pTerm :: Parser NValue
pTerm = choice
  [ parens pExpr
  , pIdent
  , pNum
  , pBool
  , pString
  , pRegex
  , pApp
  ] <?> "term"

binary :: String -> (NValue -> NValue -> NValue) -> Operator Parser NValue
binary  name f = InfixL  (f <$ lexeme (string name))

prefix, postfix :: String -> (NValue -> NValue) -> Operator Parser NValue
prefix  name f = Prefix  (f <$ lexeme (string name))
postfix name f = Postfix (f <$ lexeme (string name))

pBool :: Parser NValue
pBool = lexeme (choice
  [ NBool True <$ string "True"
  , NBool False <$ string "False"
  ] <?> "bool")

pApp :: Parser NValue
pApp = do
  char '#'
  name <- tIdent
  lexeme $ char '('
  args <- pExpr `sepBy` lexeme (char ',')
  lexeme $ char ')'
  pure $ NApp name args

pRegex :: Parser NValue
pRegex = NRegex <$> lexeme (char '/' *> manyTill L.charLiteral (char '/')) <?> "regex"

pString :: Parser NValue
pString = NString <$> lexeme (char '\'' *> manyTill L.charLiteral (char '\'')) <?> "string"

pNum :: Parser NValue
pNum = NDouble <$> L.signed sc (lexeme $ toRealFloat <$> L.scientific) <?> "double"

pIdent :: Parser NValue
pIdent = NVar <$> tIdent

tIdent :: Parser String
tIdent = lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "variable")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

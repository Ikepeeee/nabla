{-# LANGUAGE FlexibleInstances #-}

module Language.Experiment.Parser where

import Control.Monad.Trans (MonadTrans (lift))
import Data.Maybe (fromJust)
import Data.Scientific (toRealFloat)
import Data.Void
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))
import Language.Experiment.AST

type Parser = Parsec Void String

pFun :: Parser NFunc
pFun = do
  name <- tIdent
  lexeme $ char '('
  args <- pArg `sepBy` lexeme (char ',')
  lexeme $ char ')'
  lexeme $ string "->"
  expr <- pExpr
  lexeme $ string ":"
  NFunc args expr <$> pSieve

pArg :: Parser NArg
pArg = do
  arg <- tIdent
  lexeme $ string ":"
  NArg arg <$> pSieve

pSieve :: Parser NValue
pSieve = do
  lexeme $ char '{'
  argName <- tIdent
  lexeme $ char '|'
  expr <- pExpr
  lexeme $ char '}'
  pure expr

pExpr :: Parser NValue
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser NValue]]
operatorTable =
  [ [ prefix "-" (NUni "-")
    , prefix "+" id
    ]
  , [ binary "*" (NBin "*")
    , binary "/" (NBin "/")
    ]
  , [ binary "+" (NBin "+")
    , binary "-" (NBin "-")
    ]
  , [ binary ">=" (NBin ">=")
    , binary ">" (NBin ">")
    , binary "<=" (NBin "<=")
    , binary "<" (NBin "<")
    , binary "==" (NBin "==")
    , binary "/=" (NBin "/=")
    ]
  , [ prefix "!" (NUni "!")
    ]
  , [ binary "&&" (NBin "&&")
    , binary "||" (NBin "||")
    ]
  ]

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

pTerm :: Parser NValue
pTerm = choice
  [ parens pExpr
  , pIdent
  , pNum
  ]

binary :: String -> (NValue -> NValue -> NValue) -> Operator Parser NValue
binary  name f = InfixL  (f <$ lexeme (string name))

prefix, postfix :: String -> (NValue -> NValue) -> Operator Parser NValue
prefix  name f = Prefix  (f <$ lexeme (string name))
postfix name f = Postfix (f <$ lexeme (string name))

pNum :: Parser NValue
pNum = NDouble <$> L.signed sc (lexeme $ toRealFloat <$> L.scientific)

pIdent :: Parser NValue
pIdent = NDoubleVar <$> tIdent

tIdent :: Parser String
tIdent = lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "variable")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

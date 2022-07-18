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
import Language.Experiment.AST

type Parser = Parsec Void String

pFun :: Parser NFunc
pFun = do
  name <- tIdent
  lexeme $ char '('
  args <- pArg `sepBy` lexeme (char ',')
  lexeme $ char ')'
  lexeme $ string "->"
  expr <- pCondExpr
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
  expr <- pLogicExpr
  lexeme $ char '}'
  pure expr

pLogicExpr :: Parser NValue
pLogicExpr =
  try
    ( do
        a <- pCompExpr
        op <- logicOp
        op a <$> pCompExpr
    )
    <|> pCompExpr

logicOp :: Parser (NValue -> NValue -> NValue)
logicOp = lexeme $ choice [NBin <$> string "&&", NBin <$> string "||"]

pCompExpr :: Parser NValue
pCompExpr = do
  a <- pTerm
  op <- compOp
  op a <$> pTerm

compOp :: Parser (NValue -> NValue -> NValue)
compOp =
  lexeme $
    choice
      [ NBin <$> string ">=",
        NBin <$> string "<=",
        NBin <$> string "==",
        NBin <$> string "!=",
        NBin <$> string ">",
        NBin <$> string "<"
      ]

pCondExpr :: Parser NValue
pCondExpr =
  try
    (do
        c <- pLogicExpr
        lexeme $ char '?'
        t <- pExpr
        lexeme $ char ':'
        NIte c t <$> pExpr
    )
    <|> pExpr

pExpr :: Parser NValue
pExpr =
  try
    ( do
        a <- pTerm
        op <- exprOp
        op a <$> pExpr
    )
    <|> pTerm

pTerm :: Parser NValue
pTerm =
  try
    (do
        a <- pNum
        op <- termOp
        op a <$> pTerm
    )
    <|> pNum

pNum :: Parser NValue
pNum =
  pNum'
    <|> do NDoubleVar <$> tIdent
  where
    pNum' :: Parser NValue
    pNum' = NDouble <$> L.signed sc (lexeme $ toRealFloat <$> L.scientific)

exprOp :: Parser (NValue -> NValue -> NValue)
exprOp = lexeme $ choice [NBin <$> string "+", NBin <$> string "-"]

termOp :: Parser (NValue -> NValue -> NValue)
termOp =
  lexeme $
    choice
      [ NBin <$> string "*",
        NBin <$> string "/"
      ]

tIdent :: Parser String
tIdent = lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "variable")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

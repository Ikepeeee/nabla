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

fsts :: [(a, b)] -> [a]
fsts vs = map fst vs

snds :: [(a, b)] -> [b]
snds vs = map snd vs

pArg :: Parser NArg
pArg = do
  arg <- tIdent
  lexeme $ string ":"
  premise <- pSieve
  pure $ NArg arg premise

pSieve :: Parser NBool
pSieve = do
  lexeme $ char '{'
  argName <- tIdent
  lexeme $ char '|'
  expr <- pLogicExpr
  lexeme $ char '}'
  pure expr

pLogicExpr :: Parser NBool
pLogicExpr =
  try
    ( do
        a <- pCompExpr
        op <- logicOp
        op a <$> pCompExpr
    )
    <|> pCompExpr

logicOp :: Parser (NBool -> NBool -> NBool)
logicOp = lexeme $ choice [NAnd <$ string "&&", NOr <$ string "||"]

pCompExpr :: Parser NBool
pCompExpr = do
  a <- pTerm
  op <- compOp
  op a <$> pTerm

compOp :: Parser (NDouble -> NDouble -> NBool)
compOp =
  lexeme $
    choice
      [ NGe <$ string ">=",
        NLe <$ string "<=",
        NEq <$ string "==",
        NNeq <$ string "!=",
        NGt <$ string ">",
        NLt <$ string "<"
      ]

pCondExpr :: Parser NDouble
pCondExpr =
  try
    ( do
        c <- pLogicExpr
        lexeme $ char '?'
        t <- pExpr
        lexeme $ char ':'
        f <- pExpr
        pure $ NIte c t f
    )
    <|> pExpr

pExpr :: Parser NDouble
pExpr =
  try
    ( do
        a <- pTerm
        op <- exprOp
        op a <$> pExpr
    )
    <|> pTerm

pTerm :: Parser NDouble
pTerm =
  try
    (do
        a <- pNum
        op <- termOp
        op a <$> pTerm
    )
    <|> pNum

pNum :: Parser NDouble
pNum =
  pNum'
    <|> do NDoubleVar <$> tIdent
  where
    pNum' :: Parser NDouble
    pNum' = NDouble <$> L.signed sc (lexeme $ toRealFloat <$> L.scientific)

exprOp :: Parser (NDouble -> NDouble -> NDouble)
exprOp = lexeme $ choice [NAdd <$ char '+', NSub <$ char '-']

termOp :: Parser (NDouble -> NDouble -> NDouble)
termOp =
  lexeme $
    choice
      [ NMul <$ char '*',
        NDiv <$ char '/'
      ]

-- toInt :: NDouble -> SInteger
-- toInt = fromNDouble sRoundTowardZero . fpRoundToIntegral sRoundTowardZero

-- toDouble :: SInteger -> NDouble
-- toDouble = toNDouble sRoundTowardZero

tIdent :: Parser String
tIdent = lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "variable")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

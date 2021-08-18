{-# LANGUAGE FlexibleInstances #-}
module Language.Experiment.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.SBV
import Data.SBV.Dynamic
import Data.Void
import Data.Scientific (toRealFloat)
import Control.Monad.Trans (MonadTrans(lift))
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)

type Parser = ParsecT Void String Symbolic

execFun :: String -> Symbolic SBool
execFun src = do
  ret <- runParserT pFun "test" src
  case ret of
    Right b -> pure b
    Left e -> error $ errorBundlePretty e

runFun :: String -> IO ThmResult
runFun src = do
  prove $ execFun src

pFun :: Parser SBool
pFun = do
  name <- tIdent
  lexeme $ char '('
  args <- pArg `sepBy` lexeme (char ',')
  lexeme $ char ')'
  let premises = snds args
  lexeme $ string "->"
  expr <- pExpr (fsts args)
  lexeme $ string ":"
  cond <- pSieve expr
  pure $ foldr (.&&) sTrue premises .=> cond

fsts :: [(a, b)] -> [a]
fsts vs = map fst vs

snds :: [(a, b)] -> [b]
snds vs = map snd vs

pArg :: Parser ((String, SDouble), SBool)
pArg = do
  arg <- tIdent
  lexeme $ string ":"
  a <- lift $ sDouble arg
  premise <- pSieve a
  pure ((arg, a), premise)

pSieve :: SDouble -> Parser SBool
pSieve v = do
  lexeme $ char '{'
  argName <- tIdent
  lexeme $ char '|'
  expr <- pCompExpr [(argName, v)]
  lexeme $ char '}'
  pure expr

pCompExpr :: [(String, SDouble)] -> Parser SBool
pCompExpr vars = do
  a <- pTerm vars
  op <- compOp
  b <- pTerm vars
  pure $ op a b

compOp :: Parser (SDouble -> SDouble -> SBool)
compOp = lexeme $ choice
  [ (.>=) <$ string ">="
  , (.>) <$ string ">"
  , (.<) <$ string "<"
  , (.<=) <$ string "<="
  , (.==) <$ string "=="
  ]

pExpr :: [(String, SDouble)] -> Parser SDouble
pExpr vars
  = try (do
    a <- pTerm vars
    op <- exprOp
    b <- pExpr vars
    pure $ op a b)
  <|> pTerm vars

pTerm :: [(String, SDouble)] -> Parser SDouble
pTerm vars
  = try (do
    a <- pNum vars
    op <- termOp
    b <- pTerm vars
    pure $ op a b)
  <|> pNum vars

pNum :: [(String, SDouble)] -> Parser SDouble
pNum vars = L.signed sc (lexeme (literal . toRealFloat <$> L.scientific))
  <|> do
    v <- tIdent
    pure (fromJust $ lookup v vars)

exprOp :: Parser (SDouble -> SDouble -> SDouble)
exprOp = lexeme $ choice
  [ (+) <$ char '+'
  , (-) <$ char '-'
  ]

termOp :: Parser (SDouble -> SDouble -> SDouble)
termOp = lexeme $ choice
  [ (*) <$ char '*'
  ]

tIdent :: Parser String
tIdent = lexeme ((:) <$> lowerChar  <*> many alphaNumChar <?> "variable")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

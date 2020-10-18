{-# LANGUAGE OverloadedStrings #-}

module Language.Nabla.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr ()
import Language.Nabla.AST
import Data.Text (Text)

type Parser = Parsec Void Text

getPosState :: Parser (PosState Text)
getPosState = statePosState <$> getParserState

pProg :: Parser (Prog (PosState Text))
pProg = Prog <$> (scn *> many (pUnit <* scn)) <* eof

pUnit :: Parser (Unit (PosState Text))
pUnit
  = try (UnitFnType <$> pNamedFnType)
  <|> try (UnitFnDef <$> pNamedFnDef)
  <|> UnitTypeDef <$> pNamedTypeDef

pNamedTypeDef :: Parser (NamedTypeDef (PosState Text))
pNamedTypeDef = do
  name <- tIdentifier
  tDef
  td <- encloseRound' pTypeDef
  return $ NamedTypeDef name td

pTypeDef :: Parser (TypeDef (PosState Text))
pTypeDef = do
  name <- tIdentifier
  tTypeDef
  t <- pType
  tSieveDef
  s <- pExpr
  return $ TypeDef name t s

pType :: Parser (Type (PosState Text))
pType = Type <$> tIdentifier

pNamedFnDef :: Parser (NamedFnDef (PosState Text))
pNamedFnDef = do
  name <- tIdentifier
  args <- many tIdentifier
  tDef
  body <- pExpr
  return $ NamedFnDef name (Fn args body)

pNamedFnType :: Parser (NamedFnType (PosState Text))
pNamedFnType = do
  name <- tIdentifier
  tTypeDef
  fnType <- pFnType
  return $ NamedFnType name fnType

pFnType :: Parser (FnType (PosState Text))
pFnType = do
  t <- pType
  ts <- many $ tArrow *> pType
  return $ FnType (init (t:ts)) (last (t:ts))

pFn :: Parser (Fn (PosState Text))
pFn = do
  fnSt
  args <- many tIdentifier
  tArrow
  body <- pExpr
  return $ Fn args body
    where
      fnSt = L.lexeme sc $ char '\\'

pExpr :: Parser (Expr (PosState Text))
pExpr = Expr <$> getPosState <*> pValue <*> many pValue

pValue :: Parser (Value (PosState Text))
pValue
  = Alias <$> tIdentifier
  <|> Const <$> getPosState <*> pConst
  <|> FnValue <$> pFn
  <|> ExprValue <$> encloseRound pExpr

pConst :: Parser Const
pConst = Direct <$> tDirect <|> Indirect <$> intDirect

tDef :: Parser ()
tDef = L.lexeme sc $ char '=' *> pure ()

tTypeDef :: Parser ()
tTypeDef = L.lexeme sc $ string "::" *> pure ()

tSieveDef :: Parser ()
tSieveDef = L.lexeme sc $ string "|" *> pure ()

tArrow :: Parser ()
tArrow = L.lexeme sc $ string "->" *> pure ()

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p comma
  where
    comma = L.lexeme sc $ char ',' *> pure ()

encloseRound :: Parser a -> Parser a
encloseRound p = between st ed p
  where
    st = L.lexeme sc $ string "(" *> pure ()
    ed = L.lexeme sc $ string ")" *> pure ()

encloseRound' :: Parser a -> Parser a
encloseRound' p = between st ed p
  where
    st = L.lexeme sc $ string "{" *> pure ()
    ed = L.lexeme sc $ string "}" *> pure ()

tIdentifier :: Parser (Identifier (PosState Text))
tIdentifier = L.lexeme sc $ Identifier <$> getPosState <*> ((:) <$> lowerChar <*> many alphaNumChar)

tDirect :: Parser String
tDirect = try (show <$> floatT) <|> (show <$> intT) <|> dataT
  where
    dataT :: Parser String
    dataT = L.lexeme sc $ (:) <$> upperChar <*> many alphaNumChar
    intT :: Parser Integer
    intT = L.signed sc $ L.lexeme sc L.decimal
    floatT :: Parser Double
    floatT = L.signed sc $ L.lexeme sc L.float

intDirect :: Parser String
intDirect = L.lexeme sc $ (str '\'') <|> (str '"') <|> (str '`')
  where
    str :: Char -> Parser String
    str c = char c *> manyTill L.charLiteral (char c)

scn :: Parser ()
scn = L.space
  (skipSome (char ' ' <|> char '\t' <|> char '\n' <|> char '\r'))
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

sc :: Parser ()
sc = L.space
  (skipSome (char ' ' <|> char '\t'))
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Language.Nabla.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Language.Nabla.AST
import Language.Nabla.SourceSpan
import Data.Text (Text, unpack)

import Debug.Trace

type Parser = Parsec Void Text

withSourceSpan :: (SourceSpan -> a -> b) -> Parser a -> Parser b
withSourceSpan f parser = do
  stPos <- getSourcePos
  body <- parser
  edPos <- getSourcePos
  return $ f (toSourceSpan stPos edPos) body

toSourceSpan :: SourcePos -> SourcePos -> SourceSpan
toSourceSpan
  (SourcePos name stLine stCol)
  (SourcePos _ edLine edCol)
  = SourceSpan name $ TextSpan
    (TextPos (unPos stLine) (unPos stCol))
    (TextPos (unPos edLine) (unPos edCol))

parseNabla :: String -> Text -> Either (ParseErrorBundle Text Void) (Prog SourceSpan)
parseNabla = parse pProg

pProg :: Parser (Prog SourceSpan)
pProg = Prog <$> (scn *> many (pUnit <* scn)) <* eof

pUnit :: Parser (NamedUnit SourceSpan)
pUnit
  = try pNamedFnType
  <|> try pNamedFnDef
  <|> pNamedTypeDef

pNamedTypeDef :: Parser (NamedUnit SourceSpan)
pNamedTypeDef = do
  name <- tIdentifier
  tDef
  td <- encloseRound' pTypeDef
  return $ NamedUnit (name, UnitTypeDef td)

pTypeDef :: Parser (TypeDef SourceSpan)
pTypeDef = do
  name <- tIdentifier
  tTypeDef
  t <- pType
  tSieveDef
  s <- pExpr
  return $ TypeDef name t s

pType :: Parser (Type SourceSpan)
pType = Type <$> tIdentifier

pNamedFnDef :: Parser (NamedUnit SourceSpan)
pNamedFnDef = do
  name <- tIdentifier
  args <- many tIdentifier
  tDef
  body <- pExpr
  return $ NamedUnit (name, UnitFn (Fn args body))

pNamedFnType :: Parser (NamedUnit SourceSpan)
pNamedFnType = do
  name <- tIdentifier
  tTypeDef
  fnType <- pFnType
  return $ NamedUnit (name, UnitFnType fnType)

pFnType :: Parser (FnType SourceSpan)
pFnType = do
  t <- pType
  ts <- many $ tArrow *> pType
  return $ FnType (init (t:ts)) (last (t:ts))

pFn :: Parser (Fn SourceSpan)
pFn = do
  fnSt
  args <- many tIdentifier
  tArrow
  body <- pExpr
  return $ Fn args body
    where
      fnSt = L.lexeme sc $ char '\\'

pExpr :: Parser (Expr SourceSpan)
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser (Expr SourceSpan)]]
operatorTable =
  [ [ binary "*"
    , binary "/"
    ]
  , [ binary "+"
    , binary "-"
    ]
  ]

symbol :: Text -> Parser Text
symbol = L.symbol sc

binary :: Text -> Operator Parser (Expr SourceSpan)
binary name = InfixL (binaryExpr (unpack name) <$ symbol name)

binaryExpr :: String -> Expr SourceSpan -> Expr SourceSpan -> Expr SourceSpan
binaryExpr name a b = Expr undefined (Alias (Fixture name)) [a, b]

prefix, postfix :: Text -> (String -> Expr SourceSpan -> Expr SourceSpan) -> Operator Parser (Expr SourceSpan)
prefix name f = Prefix (f (unpack name) <$ symbol name)
postfix name f = Postfix (f (unpack name) <$ symbol name)

pTerm :: Parser (Expr SourceSpan)
pTerm = withSourceSpan makeExpr $
  try (do
    lOp <- pValue
    b <- tBinary
    rOp <- pValue
    return (Alias b, [lOp, rOp]))
  <|> ((,) <$> pValue <*> many pValue)
  where
    makeExpr :: SourceSpan -> (Value SourceSpan, [Value SourceSpan]) -> Expr SourceSpan
    makeExpr span (f, us) = Expr span f us

pValue :: Parser (Value SourceSpan)
pValue
  = Alias <$> tIdentifier
  <|> withSourceSpan Const pConst
  <|> FnValue <$> encloseRound pFn
  <|> ExprValue <$> encloseRound pExpr

pConst :: Parser Const
pConst = Direct <$> tDirect <|> Indirect <$> intDirect

tBinary :: Parser (Identifier SourceSpan)
tBinary = L.lexeme sc $ withSourceSpan Identifier $ choice
  [ "+" <$ string "+"
  , "-" <$ string "-"
  , "*" <$ string "*"
  , "/" <$ string "/"
  , "<" <$ string "<"
  , "<=" <$ string "<="
  , ">" <$ string ">"
  , ">=" <$ string ">="
  , "==" <$ string "=="
  ]

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

tIdentifier :: Parser (Identifier SourceSpan)
tIdentifier = L.lexeme sc $ withSourceSpan Identifier ((:) <$> lowerChar <*> many alphaNumChar)

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

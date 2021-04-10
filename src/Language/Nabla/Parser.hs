{-# LANGUAGE OverloadedStrings #-}

module Language.Nabla.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Language.Nabla.AST
import Data.Text (Text, unpack, pack)

import Debug.Trace

type Parser = Parsec Void Text

pFun :: Parser Expr
pFun = do
  lexeme (char '\\')
  arg <- tIdent
  lexeme (char ':')
  t <- pType
  lexeme (string "->")
  Fun arg t <$> pExpr

pTypedExpr :: Parser String -> Parser TypedExpr
pTypedExpr pArg = TypedExpr <$> pExpr <*> optional (lexeme (string "::") *> pType)

pType :: Parser Type
pType = choice
  [ TNum <$ lexeme (string "Num")
  , TBool <$ lexeme (string "Bool")
  ]

-- pVariable :: Parser Expr
-- pVariable = Var <$> tIdent

tIdent :: Parser String
tIdent = lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pNum :: Parser Expr
pNum = Num <$> lexeme L.decimal

pBool :: Parser Expr
pBool = Bool <$> lexeme (choice [tTrue, tFalse])
  where
    tTrue = True <$ string "True"
    tFalse =False <$ string "False"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , Var <$> tIdent
  , pNum
  , pBool
  , pFun
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix (oneOfSymbol ["-", "+", "!"]) unaryOp
    ]
  , [ binary (oneOfSymbol ["*", "/"]) binaryOp
    ]
  , [ binary (oneOfSymbol ["+", "-"]) binaryOp
    ]
  , [ binary (oneOfSymbol [">=", "<=", ">", "<", "=="]) binaryOp
    ]
  , [ binary (oneOfSymbol ["||", "&&"]) binaryOp
    ]
  , [ binary pUserBinary binaryOp
    ]
  ]

oneOfSymbol :: [Text] -> Parser Text
oneOfSymbol names = choice (map symbol names)

pUserBinary :: Parser Text
pUserBinary = symbol "`" *> (pack <$> manyTill asciiChar (symbol "`"))

binary :: Parser Text -> (String -> Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL  (f . unpack <$> name)

prefix, postfix :: Parser Text -> (String -> Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f . unpack <$> name)
postfix name f = Postfix (f . unpack <$> name)

unaryOp :: String -> Expr -> Expr
unaryOp name e = App (Var ("[unary]" <> name)) e

binaryOp :: String -> Expr -> Expr -> Expr
binaryOp name l r = App (App (Var name) l) r

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

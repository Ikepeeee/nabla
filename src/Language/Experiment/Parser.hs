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
  (argName, t, cond) <- pSieve
  pure $ NFunc args expr argName t cond

pArg :: Parser NArg
pArg = do
  arg <- tIdent
  lexeme $ string ":"
  (_, t, cond) <- pSieve
  pure $ NArg arg t cond


pSieve :: Parser (String, TypeName, NValue)
pSieve = do
  lexeme $ char '{'
  argName <- tIdent
  lexeme $ char ':'
  t <- lexeme $ string "Double" <|> string "Bool" <|> string "String"
  lexeme $ char '|'
  expr <- pExpr
  lexeme $ char '}'
  pure (argName, t, expr)

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
    , binary "!=" (NBin "/=")
    , binary "~=" (NBin "~=")
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
  , pBool
  , pString
  , pRegex
  ]

binary :: String -> (NValue -> NValue -> NValue) -> Operator Parser NValue
binary  name f = InfixL  (f <$ lexeme (string name))

prefix, postfix :: String -> (NValue -> NValue) -> Operator Parser NValue
prefix  name f = Prefix  (f <$ lexeme (string name))
postfix name f = Postfix (f <$ lexeme (string name))

pBool :: Parser NValue
pBool = lexeme $ choice
  [ NBool True <$ string "True"
  , NBool False <$ string "False"
  ]

pRegex :: Parser NValue
pRegex = NRegex <$> lexeme (char '/' *> manyTill L.charLiteral (char '/'))

pString :: Parser NValue
pString = NString <$> lexeme (char '\'' *> manyTill L.charLiteral (char '\''))

pNum :: Parser NValue
pNum = NDouble <$> L.signed sc (lexeme $ toRealFloat <$> L.scientific)

pIdent :: Parser NValue
pIdent = NVar <$> tIdent

tIdent :: Parser String
tIdent = lexeme ((:) <$> lowerChar <*> many alphaNumChar <?> "variable")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

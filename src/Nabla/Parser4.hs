-- module Nabla.memo.Parser where

--   import Control.Monad.IO.Class
--   import Data.Void
--   import Text.Megaparsec
--   import Text.Megaparsec.Debug (dbg)
--   import Text.Megaparsec.Char
--   import qualified Text.Megaparsec.Char.Lexer as L
--   import Nabla.AST

--   type Parser = Parsec Void String

--   ast :: Parser AST
--   ast = AST <$> many (expr <* some (newlineT <|> semiT)) <* eof

--   expr :: Parser Expr
--   expr = try assign <|> apply
--     where
--       apply :: Parser Expr
--       apply = Apply <$> some value
--       assign :: Parser Expr
--       assign = Assign <$> identifierT <*> (assignT *> value)

--   value :: Parser Value
--   value
--     = SimpleV <$> simpleValue
--     <|> IdentifierV <$> identifierT
--     -- <|> try (FunctionV <$> function)
--     <|> leftT *> (ExprV <$> expr) <* rightT

--   simpleValue :: Parser SimpleValue
--   simpleValue
--     = NumberV <$> numberT
--     <|> StringV <$> stringT
--     <|> SymbolV <$> symbolT

--   -- function :: Parser Function
--   -- function = Function <$> args <*> (rightArrow *> body)
--   --   where
--   --     body :: Parser AST
--   --     body = leftT' *> (AST <$> many (expr <* semiT)) <* rightT'
--   --     args :: Parser [Identifier]
--   --     args = leftT *> many identifierT <* rightT

--   variable :: Parser Variable
--   variable = Variable <$> identifierT <*> value

--   identifierT :: Parser Identifier
--   identifierT = L.lexeme sc $ (:) <$> lowerChar <*> many alphaNumChar

--   numberT :: Parser String
--   numberT = try (show <$> floatT) <|> (show <$> intT)
--     where
--       intT :: Parser Integer
--       intT = L.signed sc $ L.lexeme sc L.decimal
--       floatT :: Parser Double
--       floatT = L.signed sc $ L.lexeme sc L.float

--   stringT :: Parser String
--   stringT = L.lexeme sc (char '\'' *> manyTill L.charLiteral (char '\''))

--   symbolT :: Parser String
--   symbolT = L.lexeme sc (char ':' *> some alphaNumChar)

--   newlineT :: Parser String
--   newlineT = L.lexeme sc $ (:[]) <$> newline

--   semiT :: Parser String
--   semiT = L.lexeme sc $ string ";"

--   rightT :: Parser String
--   rightT = L.lexeme sc $ string ")"

--   leftT :: Parser String
--   leftT = L.lexeme sc $ string "("

--   rightT' :: Parser String
--   rightT' = L.lexeme sc $ string "}"

--   leftT' :: Parser String
--   leftT' = L.lexeme sc $ string "{"

--   rightArrow :: Parser String
--   rightArrow = L.lexeme sc $ string "->"

--   assignT :: Parser String
--   assignT = L.lexeme sc $ string "="

--   equalT :: Parser String
--   equalT = L.lexeme sc $ string "=="

--   sc :: Parser ()
--   sc = L.space
--     (skipSome (char ' ' <|> char '\t'))
--     (L.skipLineComment "//")
--     (L.skipBlockComment "/*" "*/")

--   scn :: Parser ()
--   scn = L.space
--     space1
--     (L.skipLineComment "//")
--     (L.skipBlockComment "/*" "*/")

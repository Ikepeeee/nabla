module Language.Nabla.Helper where

import Data.Text (Text, unpack, pack)
import Text.Megaparsec (parse, errorBundlePretty)
import Language.Nabla.TypeChecker (valid)
import Language.Nabla.AST (Type)
import Language.Nabla.Parser (pTypedExpr)
import Language.Nabla.Fixture (fixtureTypeEnv)

parseAndInfer :: String -> Either String Type
parseAndInfer src =
  case parse pTypedExpr "test" (pack src) of
    Right e -> case valid fixtureTypeEnv e of
      (Right t) -> pure t
      (Left e) -> Left $ show e
    Left e -> Left $ errorBundlePretty e

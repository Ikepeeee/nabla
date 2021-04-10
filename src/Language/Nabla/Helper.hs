module Language.Nabla.Helper where

import Data.Text (Text, unpack, pack)
import Text.Megaparsec (parse, errorBundlePretty)
import Language.Nabla.TypeChecker (infer)
import Language.Nabla.AST (Type)
import Language.Nabla.Parser (pExpr)
import Language.Nabla.Fixture (fixtureTypeEnv)

parseAndInfer :: String -> Either String Type
parseAndInfer src =
  case parse pExpr "test" (pack src) of
    Right e -> Right $ infer fixtureTypeEnv e
    Left e -> Left $ errorBundlePretty e

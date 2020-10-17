module Test where

import System.Environment (getArgs)
import qualified Data.Text.IO as DTI
import Text.Megaparsec (parse)
import Text.Megaparsec.Error ( errorBundlePretty )
import Text.Megaparsec.Pos ( sourcePosPretty )
import Language.Nabla.AST ( Identifier(Identifier) )
import Language.Nabla.Parser (pProg)
import Language.Nabla.TypeChecker
    ( TypeCheck(valid), TypeValidationError(NameNotFound), getPos)

main :: IO ()
main = do
  [fileName] <- getArgs
  src <- DTI.readFile fileName
  case parse pProg fileName src of
    Right prog -> case (valid prog []) of
      Right () -> print prog
      Left e -> putStrLn $ sourcePosPretty (getPos e) <> "\n" <> show e
    Left e -> putStrLn $ errorBundlePretty e
  return ()

{-# LANGUAGE FlexibleInstances #-}

module Test where

import System.Environment (getArgs)
import qualified Data.Text.IO as DTI
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Data.Set (singleton)
import Text.Megaparsec (parse, PosState)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos ( sourcePosPretty )
import Language.Nabla.AST ( Identifier(Identifier) )
import Language.Nabla.Parser (pProg)
import Language.Nabla.TypeChecker
    ( TypeCheck(valid), TypeValidationError(NameNotFound), getPos )

main :: IO ()
main = do
  [fileName] <- getArgs
  src <- DTI.readFile fileName
  case parse pProg fileName src of
    Right prog -> case (valid prog []) of
      Right () -> print prog
      Left e -> putStrLn $ errorBundlePretty $ customError e
    Left e -> putStrLn $ errorBundlePretty e
  return ()


customError :: TypeValidationError (PosState s) -> ParseErrorBundle s String
customError e = ParseErrorBundle ((FancyError 0 (singleton $ ErrorFail (show e))) :| []) (getPos e)

instance ShowErrorComponent String where
  showErrorComponent = show

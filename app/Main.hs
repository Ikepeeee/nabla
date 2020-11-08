{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as DTI
import Data.List (intercalate)
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Data.Set (singleton)
import Text.Megaparsec (parse, PosState)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos ( sourcePosPretty )
import Language.Nabla.AST ( Identifier(Identifier) )
import Language.Nabla.Parser (pProg)
import Language.Nabla.TypeChecker
    ( TypeCheck(valid), TypeValidationError(NameNotFound), getPos )
import Language.Nabla.SourceSpan

main :: IO ()
main = do
  [fileName] <- getArgs
  src <- DTI.readFile fileName
  case parse pProg fileName src of
    Right prog -> case (valid prog []) of
      [] -> print prog
      es -> putStrLn $ intercalate "\n\n" $ map
        (\e -> show (getPos e) <> "\n" <> textSpanPretty (textSpan $ getPos e) src <> show e)
        es
    Left e -> putStrLn $ errorBundlePretty e
  return ()

instance ShowErrorComponent String where
  showErrorComponent = show

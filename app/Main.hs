{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as DTI
import Data.List (intercalate)
import Text.Megaparsec (parse)
import Text.Megaparsec.Error
import Language.Nabla.Parser (pProg)
import Language.Nabla.TypeChecker (check, getPos)
import Language.Nabla.SourceSpan

main :: IO ()
main = do
  [fileName] <- getArgs
  src <- DTI.readFile fileName
  case parse pProg fileName src of
    Right prog -> case check prog of
      [] -> print prog
      es -> putStrLn $ intercalate "\n\n" $ map
        (\e -> show (getPos e) <> "\n" <> textSpanPretty (textSpan $ getPos e) src <> show e)
        es
    Left e -> putStrLn $ errorBundlePretty e
  return ()

instance ShowErrorComponent String where
  showErrorComponent = show

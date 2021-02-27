module Language.Nabla.Compiler where

import System.FilePath.Posix (replaceExtension)
import qualified Data.Text as DT
import Text.Megaparsec (parse)
import Data.List (intercalate)
import Text.Megaparsec.Error
import Language.Nabla.Parser (pProg)
import Language.Nabla.TypeChecker (check, getPos)
import Language.Nabla.Printer (printJS)
import Language.Nabla.SourceSpan

import Debug.Trace (trace)

trace' :: (Show a) => String -> a -> a
trace' m a = trace (m <> show a) a

-- TODO compile nabla source to JavaScript code

compile :: [(FilePath, DT.Text)] -> Either String [(FilePath, DT.Text)]
compile [(fileName, src)] = case parse pProg fileName src of
    Right prog -> case check prog of
      [] -> Right [(replaceExtension fileName "js", printJS prog)]
      es -> Left $ intercalate "\n\n" $ map
        (\e -> show (getPos e) <> "\n" <> textSpanPretty (textSpan $ getPos e) src <> show e)
        es
    Left e -> Left $ errorBundlePretty e

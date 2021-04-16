module Infer where

import Language.Nabla.Helper
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case parseAndInfer (args !! 0) of
    Right t -> print t
    Left e -> putStrLn e

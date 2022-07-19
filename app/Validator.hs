module Validator where

import Language.Nabla.Helper
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  result <- runFun (args !! 0)
  putStrLn $ show result

module Transpile where

import Language.Nabla.Helper
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  trans (args !! 0)

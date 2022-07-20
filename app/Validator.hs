module Validator where
import Data.List
import Language.Nabla.Helper
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  result <- runFun (args !! 0)
  putStrLn $ intercalate "\n" (map show result)

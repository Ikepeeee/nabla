module TypeValidator where
import Data.List
import Language.Nabla.Helper
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  valid (args !! 0)

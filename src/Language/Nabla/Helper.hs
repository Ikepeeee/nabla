module Language.Nabla.Helper where
import Data.List
import Data.SBV ( proveWith, z3, SBool, Symbolic, ThmResult )
import Text.Megaparsec ( runParser, errorBundlePretty )
import Language.Nabla.Parser ( pFuns )
import Language.Nabla.Printer
import Language.Nabla.TypeChecker ( createCond )

execFun :: String -> [Symbolic SBool]
execFun src = do
  let ret = runParser pFuns "test" src
  case ret of
    Right fs -> map (createCond fs . snd) fs
    Left e -> error $ errorBundlePretty e

runFun :: String -> IO [ThmResult]
runFun src = do
  mapM (proveWith z3) (execFun src) -- cvc4 or mathsat or z3

trans :: String -> IO ()
trans src = do
  let ret = runParser pFuns "test" src
  case ret of
    Right fs -> putStrLn $ intercalate "\n" $ map (transFunc . snd) fs
    Left e -> error $ errorBundlePretty e

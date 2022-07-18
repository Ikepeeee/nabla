module Language.Experiment.Helper where

import Data.SBV ( proveWith, z3, SBool, Symbolic, ThmResult )
import Text.Megaparsec ( runParser, errorBundlePretty )
import Language.Experiment.Parser ( pFun )
import Language.Experiment.Transpiler
import Language.Experiment.TypeChecker ( createCond )

execFun :: String -> Symbolic SBool
execFun src = do
  let ret = runParser pFun "test" src
  case ret of
    Right b -> createCond b
    Left e -> error $ errorBundlePretty e

runFun :: String -> IO ThmResult
runFun src = do
  proveWith z3 $ execFun src -- cvc4 or mathsat or z3

-- trans :: String -> IO ()
-- trans src = do
--   let ret = runParser pFun "test" src
--   case ret of
--     Right b -> putStrLn $ transFunc b
--     Left e -> error $ errorBundlePretty e

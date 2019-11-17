import System.Exit
import Test.HUnit
import System.IO
import ParserTest

main = do
  counts <- runTestTT tests
  if (errors counts) + (failures counts) > 0
    then exitWith $ ExitFailure 1
    else exitSuccess

tests = TestList $ concat
  [ simpleValueTest
  , complexValueTest
  , variableExprTest
  , assignExprTest
  ]

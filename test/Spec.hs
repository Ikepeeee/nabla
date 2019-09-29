import Test.HUnit
import System.IO
import ParserTest

main = runTestText (putTextToHandle stderr False) tests

tests = TestList $ concat
  [ simpleValueTest
  , complexValueTest
  , variableExprTest
  , assignExprTest
  ]

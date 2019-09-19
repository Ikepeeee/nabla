import Test.HUnit
import System.IO
import Text.Megaparsec (parse)
import Nabla.Parser
import Nabla.AST

main = runTestText (putTextToHandle stderr False) tests

tests = TestList simpleValueTest

simpleV t str = Right (AST [(ValueExpr (SimpleV (t str)))])
simpleValueTest =
  [ "number" ~: parse ast "" "1\n" ~?= simpleV NumberV "1"
  , "number" ~: parse ast "" "+1\n" ~?= simpleV NumberV "1"
  , "number" ~: parse ast "" "-1\n" ~?= simpleV NumberV "-1"
  , "number" ~: parse ast "" "1.0\n" ~?= simpleV NumberV "1.0"
  , "number" ~: parse ast "" "+1.0\n" ~?= simpleV NumberV "1.0"
  , "number" ~: parse ast "" "-1.0\n" ~?= simpleV NumberV "-1.0"
  , "string" ~: parse ast "" "'abc'\n" ~?= simpleV StringV "abc"
  , "string" ~: parse ast "" "'a b c'\n" ~?= simpleV StringV "a b c"
  , "string" ~: parse ast "" "'あいうえお'\n" ~?= simpleV StringV "あいうえお"
  , "string" ~: parse ast "" "''\n" ~?= simpleV StringV ""
  , "string" ~: parse ast "" "' '\n" ~?= simpleV StringV " "
  , "symbol" ~: parse ast "" ":a\n" ~?= simpleV SymbolV "a"
  , "symbol" ~: parse ast "" ":A\n" ~?= simpleV SymbolV "A"
  , "symbol" ~: parse ast "" ":abc\n" ~?= simpleV SymbolV "abc"
  , "symbol" ~: parse ast "" ":ABC\n" ~?= simpleV SymbolV "ABC"
  , "symbol" ~: parse ast "" ":1\n" ~?= simpleV SymbolV "1"
  , "symbol" ~: parse ast "" ":123\n" ~?= simpleV SymbolV "123"
  ]

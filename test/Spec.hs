import Test.HUnit
import System.IO
import Text.Megaparsec (parse)
import Nabla.Parser
import Nabla.AST

main = runTestText (putTextToHandle stderr False) tests

tests = TestList $ concat
  [ simpleValueTest
  , complexValueTest
  ]

simpleV t str = Right (AST [(ValueExpr (SimpleV (t str)))])
complexV str vs = Right (AST [(ValueExpr (ComplexV (WrapValues str vs)))])

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

complexValueTest =
  [ "complex" ~: parse ast "" "Nothing\n" ~?= complexV "Nothing" []
  , "complex" ~: parse ast "" "Just 1\n" ~?=
      complexV "Just"
        [ SimpleV (NumberV "1")
        ]
  , "complex" ~: parse ast "" "Just Just 1\n" ~?=
    complexV "Just"
      [ ComplexV (WrapValues "Just"
          [ SimpleV (NumberV "1") ]
        )
      ]
  , "complex" ~: parse ast "" "List 1 2\n" ~?=
      complexV "List"
        [ SimpleV (NumberV "1")
        , SimpleV (NumberV "2")
        ]
  , "complex" ~: parse ast "" "List Just 1\n" ~?=
      complexV "List"
        [ ComplexV (WrapValues "Just"
            [ SimpleV (NumberV "1")
            ]
          )
        ]
  , "complex" ~: parse ast "" "List Just 'a' Just 'b'\n" ~?=
      complexV "List"
      [ ComplexV (WrapValues "Just"
          [ SimpleV (StringV "a")
          , ComplexV (WrapValues "Just"
              [ SimpleV (StringV "b")
              ]
            )
          ]
        )
      ]
  ]

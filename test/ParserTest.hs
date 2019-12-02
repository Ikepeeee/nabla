module ParserTest where

import Test.HUnit
import Text.Megaparsec (parse)
import Nabla.Parser
import Nabla.AST

simpleV t str = ValueExpr (SimpleV (t str))
complexV str vs = ValueExpr (ComplexV (WrapValues str vs))

simpleValueTest =
  [ "number" ~: parse ast "" "1\n" ~?= Right (AST [(simpleV NumberV "1")])
  , "number" ~: parse ast "" "+1\n" ~?= Right (AST [(simpleV NumberV "1")])
  , "number" ~: parse ast "" "-1\n" ~?= Right (AST [(simpleV NumberV "-1")])
  , "number" ~: parse ast "" "1.0\n" ~?= Right (AST [(simpleV NumberV "1.0")])
  , "number" ~: parse ast "" "+1.0\n" ~?= Right (AST [(simpleV NumberV "1.0")])
  , "number" ~: parse ast "" "-1.0\n" ~?= Right (AST [(simpleV NumberV "-1.0")])
  , "string" ~: parse ast "" "'abc'\n" ~?= Right (AST [(simpleV StringV "abc")])
  , "string" ~: parse ast "" "'a b c'\n" ~?= Right (AST [(simpleV StringV "a b c")])
  , "string" ~: parse ast "" "'あいうえお'\n" ~?= Right (AST [(simpleV StringV "あいうえお")])
  , "string" ~: parse ast "" "''\n" ~?= Right (AST [(simpleV StringV "")])
  , "string" ~: parse ast "" "' '\n" ~?= Right (AST [(simpleV StringV " ")])
  , "symbol" ~: parse ast "" ":a\n" ~?= Right (AST [(simpleV SymbolV "a")])
  , "symbol" ~: parse ast "" ":A\n" ~?= Right (AST [(simpleV SymbolV "A")])
  , "symbol" ~: parse ast "" ":abc\n" ~?= Right (AST [(simpleV SymbolV "abc")])
  , "symbol" ~: parse ast "" ":ABC\n" ~?= Right (AST [(simpleV SymbolV "ABC")])
  , "symbol" ~: parse ast "" ":1\n" ~?= Right (AST [(simpleV SymbolV "1")])
  , "symbol" ~: parse ast "" ":123\n" ~?= Right (AST [(simpleV SymbolV "123")])
  ]

complexValueTest =
  [ "complex" ~: parse ast "" "Nothing\n" ~?= Right (AST [(complexV "Nothing" [])])
  , "complex" ~: parse ast "" "Just 1\n" ~?=
    Right (AST [ (complexV "Just" [simpleV NumberV "1"]) ])
  , "complex" ~: parse ast "" "Just Just 1\n" ~?=
      Right (AST
        [ (complexV "Just"
            [ complexV "Just" [ simpleV NumberV "1" ] ]
          )
        ]
      )
  , "complex" ~: parse ast "" "List 1 2\n" ~?=
      Right (AST
        [ (complexV "List"
            [ simpleV NumberV "1"
            , simpleV NumberV "2"
            ]
          )
        ]
      )
  , "complex" ~: parse ast "" "List Just 1\n" ~?=
      Right (AST
        [ (complexV "List"
            [ complexV "Just" [ simpleV NumberV "1" ]
            ]
          )
        ]
      )
  , "complex" ~: parse ast "" "List Just 'a' Just 'b'\n" ~?=
      Right (AST
        [ (complexV "List"
            [ complexV "Just"
              [ simpleV StringV "a"
              , complexV "Just"
                [ simpleV StringV "b"
                ]
              ]
            ]
          )
        ]
      )
  , "complex" ~: parse ast "" "List (Just 'a') (Just 'b')\n" ~?=
    Right (AST
      [ (complexV "List"
          [ complexV "Just" [ simpleV StringV "a" ]
          , complexV "Just" [ simpleV StringV "b" ]
          ]
        )
      ]
    )
  ]

variableExprTest =
  [ "assign" ~: parse ast "" "a\n" ~?=
    Right (AST [(VariableExpr "a")])
  , "assign" ~: parse ast "" "a1\n" ~?=
    Right (AST [(VariableExpr "a1")])
  , "assign" ~: parse ast "" "aA\n" ~?=
    Right (AST [(VariableExpr "aA")])
  , "assign" ~: parse ast "" "a1A\n" ~?=
    Right (AST [(VariableExpr "a1A")])
  ]

assignExprTest =
  [ "assign" ~: parse ast "" "a = 1\n" ~?= Right
    (AST [
      (Assign "a"
        (ValueExpr
          (SimpleV (NumberV "1"))
        )
      )
    ])
  , "assign" ~: parse ast "" "a = Just 1\n" ~?= Right
    (AST [
      (Assign "a"
        (ValueExpr
          (ComplexV
            (WrapValues "Just" [ValueExpr (SimpleV (NumberV "1"))])
          )
        )
      )
    ])
  , "assign" ~: parse ast "" "a = b\n" ~?= Right
    (AST [
      (Assign "a"
        (VariableExpr "b")
      )
    ])
  ]

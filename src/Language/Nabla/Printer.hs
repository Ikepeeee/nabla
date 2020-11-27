module Language.Nabla.Printer (printJS) where

import Data.Text (Text, pack)
import Data.List (intercalate)
import Language.Nabla.AST

printJS :: Prog p -> Text
printJS (Prog us) = pack $ join "\n" printJSNamedUnit us

printJSNamedUnit :: NamedUnit p -> String
printJSNamedUnit (NamedUnit (name, UnitFn fn)) = "const " <> show name <> " = " <> printJSFn fn <> ";"
printJsNamedUnit _ = ""

printJSFn :: Fn p -> String
printJSFn (Fn args body)
  = "(" <> join ", " show args <> ")" <> " => " <> printJSExpr body

printJSExpr :: Expr p -> String
printJSExpr (Expr _ v []) = printJSValue v
printJSExpr (Expr _ v vs) = printJSValue v <> "(" <> join ", " printJSValue vs <> ")"

printJSValue :: Value p -> String
printJSValue (Alias name) = show name
printJSValue (FnValue fn) = "(" <> printJSFn fn <> ")"
printJSValue (Const _ c) = show c
printJSValue (ExprValue expr) ="(" <> printJSExpr expr <> ")"

join :: String -> (a -> String) -> [a]  -> String
join separator toS vs = intercalate separator (map toS vs)

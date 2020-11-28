module Language.Nabla.Printer (printJS) where

import Data.Text (Text, pack)
import Data.List (intercalate)
import Language.Nabla.AST
import Language.Nabla.Fixture

printJS :: Prog p -> Text
printJS (Prog us) = pack $ join "\n" printJSNamedUnit us

printJSNamedUnit :: NamedUnit p -> String
printJSNamedUnit (NamedUnit (name, UnitFn fn)) = "const " <> show name <> " = " <> printJSFn fn <> ";"
printJSNamedUnit _ = ""

printJSFn :: Fn p -> String
printJSFn (Fn [] body) = "() => " <> printJSExpr body
printJSFn (Fn args body)
  = concatMap (\arg -> "(" <> show arg <> ")" <> " => ") args <> printJSExpr body

printJSExpr :: Expr p -> String
printJSExpr (Expr _ v []) = printJSValue v
printJSExpr (Expr _ v vs) = case v of
  (Alias name) -> case lookup name fixtureFns of
    Just printJSFixture -> printJSFixture (map printJSValue vs)
    Nothing -> printJSValue v <> concatMap (\arg -> "(" <> show arg <> ")") vs
  _ -> printJSValue v <> concatMap (\arg -> "(" <> show arg <> ")") vs

printJSValue :: Value p -> String
printJSValue (Alias name) = show name
printJSValue (FnValue fn) = "(" <> printJSFn fn <> ")"
printJSValue (Const _ c) = show c
printJSValue (ExprValue expr) ="(" <> printJSExpr expr <> ")"

join :: String -> (a -> String) -> [a]  -> String
join separator toS vs = intercalate separator (map toS vs)

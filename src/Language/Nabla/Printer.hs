module Language.Nabla.Printer where

import Language.Nabla.AST

data JSCode
  = JSExpr String
  | JSFun (String -> JSCode)

printExpr :: Expr -> JSCode
printExpr (Num n) = JSExpr $ show n
printExpr (Bool e)
  | True = JSExpr "true"
  | False = JSExpr "false"
printExpr (Var "+") = jsBinary "+"
printExpr (Var name) = JSExpr name
printExpr (App f x) = do
  let f' = printExpr f
  let (JSExpr e) = printExpr x
  case f' of
    (JSFun f'') -> f'' e
-- printExpr (Fun )

jsBinary :: String -> JSCode
jsBinary op = JSFun $ \a -> JSFun $ \b -> JSExpr $ "(" <> a <> op <> b <> ")"

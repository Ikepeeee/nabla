module Utils.ASTPreset where

import Language.Nabla.AST

zero :: Expr
zero = Num 0
n1 :: Expr
n1 = Num 1
n1' :: Expr
n1' = App (Var "[unary]-") (Num 1)

app2 :: Expr -> Expr -> Expr -> Expr
app2 f a b = App (App f a) b

posSieve :: Sieve
posSieve = Sieve TNum $ Fun "n" $ app2 (Var ">=") (Var "n") (Num 0)

rangeSieve :: Double -> Double -> Sieve
rangeSieve min max = Sieve TNum $ Fun "n"
  $ app2 (Var "&&")
    (app2 (Var "<=") (Num min) (Var "n"))
    (app2 (Var "<=") (Var "n") (Num max))

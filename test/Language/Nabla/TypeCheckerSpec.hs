module Language.Nabla.TypeCheckerSpec (spec) where

import Test.Hspec
import Language.Nabla.TypeChecker
import Language.Nabla.AST
import Language.Nabla.Fixture

spec :: Spec
spec = do
  describe "valid function" $ do
    it "'0 :: { n :: Num | n >= 0 }' is OK"
      $ valid fixtureTypeEnv (TypedExpr zero (Just posSieve))
        `shouldBe` Right TNum
    it "'-1 :: { n :: Num | n >= 0 }' is NG"
      $ valid fixtureTypeEnv (TypedExpr n1' (Just posSieve))
        `shouldBe` Left (UnmatchableSieveError n1' posSieve)

zero :: Expr
zero = Num 0
n1' :: Expr
n1' = Num (-1)

posSieve :: Sieve
posSieve = Sieve $ Fun "n" TNum $ App (App (Var ">=") (Var "n")) (Num 0)

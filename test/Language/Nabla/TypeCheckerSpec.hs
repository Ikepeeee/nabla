module Language.Nabla.TypeCheckerSpec (spec) where

import Test.Hspec
import Language.Nabla.TypeChecker
import Language.Nabla.AST
import Language.Nabla.Fixture
import Utils.ASTPreset
import Debug.Trace

spec :: Spec
spec = do
  describe "infer function" $ do
    it "0 is Num"
      $ infer fixtureTypeEnv zero `shouldBe` Right TNum
    it "\\n -> 1 + n is Num -> Num"
      $ infer fixtureTypeEnv (Fun "n" (app2 (Var "+") (Num 1) (Var "n"))) `shouldBe` Right (TFun TNum TNum)
    it "\\n -> n + 1 is Num -> Num"
      $ infer fixtureTypeEnv (Fun "n" (app2 (Var "+") (Var "n") (Num 1))) `shouldBe` Right (TFun TNum TNum)
    it "\\a -> a is T1 -> T1"
      $ infer fixtureTypeEnv (Fun "a" (Var "a")) `shouldBe` Right (TFun (TVar 1) (TVar 1))
    it "\\a -> \\b -> a is T1 -> T2 -> T1"
      $ infer fixtureTypeEnv (Fun "a" (Fun "b" (Var "a"))) `shouldBe` Right (TFun (TVar 1) (TFun (TVar 2) (TVar 1)))
    it "\\a -> \\b -> a * b is Num -> Num -> Num"
      $ infer fixtureTypeEnv (Fun "a" (Fun "b" (app2 (Var "+") (Var "a") (Var "b")))) `shouldBe` Right (TFun TNum (TFun TNum TNum))
    it "(\\a -> a) 1 is Num"
      $ infer fixtureTypeEnv (App (Fun "a" (Var "a")) (Num 1)) `shouldBe` Right TNum
    -- it "(\\a -> a) (\\b -> b) is T2 -> T2"
    --   $ infer fixtureTypeEnv (App (Fun "a" (Var "a")) (Fun "b" (Var "b"))) `shouldBe` Right (TFun (TVar 2) (TVar 2))
    -- it "(\\a -> a) (\\a -> a + 1) is Num -> Num"
    --   $ infer fixtureTypeEnv (App (Fun "a" (Var "a")) (Fun "a" (app2 (Var "+") (Var "a") (Num 1)))) `shouldBe` Right (TFun TNum TNum)
  describe "valid function" $ do
    it "'0 : { n : Num | n >= 0 }' is OK"
      $ valid fixtureTypeEnv (TypedExpr zero (Just posSieve))
        `shouldBe` Right TNum
    it "'-1 : { n : Num | n >= 0 }' is NG"
      $ valid fixtureTypeEnv (TypedExpr n1' (Just posSieve))
        `shouldBe` Left (UnmatchableSieveError n1' posSieve)
    it "'0.5 : { n : Num | 0 <= n && n <= 1 }' is OK"
      $ valid fixtureTypeEnv (TypedExpr (Num 0.5) (Just $ rangeSieve 0 1))
        `shouldBe` Right TNum
    it "'2 : { n : Num | 0 <= n && n <= 1 }' is NG"
      $ valid fixtureTypeEnv (TypedExpr (Num 2) (Just $ rangeSieve 0 1))
        `shouldBe` Left (UnmatchableSieveError (Num 2) (rangeSieve 0 1))
    it "'1 + 1 : { n : Num | n >= 0 }' is OK"
      $ valid fixtureTypeEnv (TypedExpr (app2 (Var "+") n1 n1) (Just posSieve))
        `shouldBe` Right TNum
    it "'1 - 2 : { n : Num | n >= 0 }' is NG"
      $ valid fixtureTypeEnv (TypedExpr (app2 (Var "-") n1 (Num 2)) (Just posSieve))
        `shouldBe` Left (UnmatchableSieveError (app2 (Var "-") n1 (Num 2)) posSieve)
    it "'\\n -> n + 1 : { f : Num -> Num | f 2 == 3 }' is OK"
      $ valid fixtureTypeEnv (TypedExpr (Fun "n" (app2 (Var "+") (Var "n") (Num 1))) (Just $ Sieve (TFun TNum TNum) (Fun "f" (app2 (Var "==") (App (Var "f") (Num 2)) (Num 3)))))
        `shouldBe` Right (TFun TNum TNum)
    -- it "'\\n : Num -> n + 1 : Num -> { n : Num | f 2 == 4 }' is NG"
    --   $ valid fixtureTypeEnv (TypedExpr (Fun "n" (app2 (Var "+") (Var "n") (Num 1))) (Just $ Sieve (TFun TNum TNum) (Fun "f"  (app2 (Var "==") (App (Var "f") (Num 2)) (Num 4)))))
    --     `shouldBe` Left (UnmatchableSieveError (Fun "n" (app2 (Var "+") (Var "n") (Num 1))) (Sieve (TFun TNum TNum) (Fun "f" (app2 (Var "==") (App (Var "f") (Num 2)) (Num 4)))))

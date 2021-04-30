module Language.Nabla.TypeCheckerSpec (spec) where

import Test.Hspec
import Language.Nabla.TypeChecker
import Language.Nabla.AST
import Language.Nabla.Fixture
import Utils.ASTPreset

spec :: Spec
spec = do
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


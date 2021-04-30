{-# LANGUAGE OverloadedStrings #-}
module Language.Nabla.ParserSpec (spec) where

import Test.Hspec
import Data.Text
import Language.Nabla.AST
import Language.Nabla.Parser
import Text.Megaparsec (parse)
import Utils.ASTPreset

spec :: Spec
spec = do
  describe "pTypedExpr function" $ do
    it "0 : { n : Num | n >= 0 }"
      $ parseTypedExpr "test" "0 : { n : Num | n >= 0 }" `shouldBe` Right (TypedExpr zero (Just posSieve))
    it "-1 : { n : Num | n >= 0 }"
      $ parseTypedExpr "test" "-1 : { n : Num | n >= 0 }" `shouldBe` Right (TypedExpr n1' (Just posSieve))
    it "0.5 : { n : Num | 0 <= n && n <= 1 }"
      $ parseTypedExpr "test" "0.5 : { n : Num | 0 <= n && n <= 1 }" `shouldBe` Right (TypedExpr (Num 0.5) (Just $ rangeSieve 0 1))

parseTypedExpr = parse pTypedExpr

module Language.Nabla.CompilerSpec (spec) where

import Test.Hspec
import Data.Text

import Language.Nabla.Compiler

spec :: Spec
spec = do
  describe "Compile" $ do
    it "Simple Function" $
      compile [("test.nb", simpleFunctionSrc)] `shouldBe` Right [("test.js", simpleFunctionDist)]
    it "Simple 2 Functions" $
      compile [("test.nb", simple2FunctionsSrc)] `shouldBe` Right [("test.js", simple2FunctionsDist)]
  describe "Error" $ do
    it "Not Defined Function" $
      compile [("test.nb", notDefinedFunctionSrc)] `shouldBe` Left notDefinedFunctionErrorMessage
    it "Not Defined Type" $
      compile [("test.nb", notDefinedTypeSrc)] `shouldBe` Left notDefinedTypeErrorMessage
    it "Incorrect Const Argument Type" $
      compile [("test.nb", incorrectConstArgumentTypeSrc)] `shouldBe` Left incorrectConstArgumentTypeErrorMessage
    it "Incorrect Alias Argument Type" $
      compile [("test.nb", incorrectAliasArgumentTypeSrc)] `shouldBe` Left incorrectAliasArgumentTypeErrorMessage
    it "Incorrect Function Type" $
      compile [("test.nb", incorrectFunctionTypeSrc)] `shouldBe` Left incorrectFunctionTypeErrorMessage

simpleFunctionSrc = pack "f a b = a + b"
simpleFunctionDist = pack "const f = (a, b) => a + b;"

simple2FunctionsSrc
  = pack
  "id a = a\n\
  \g = id 2"
simple2FunctionsDist
  = pack
  "const id = (a) => a;\n\
  \const g = () => id(2);"


notDefinedFunctionSrc = pack "f a = foo a"
notDefinedFunctionErrorMessage
  = "\"test.nb\":1:7-1:10\n\
    \ |\n\
    \ | f a = foo a\n\
    \ |       ^^^\n\
    \name 'foo' is not defined"

notDefinedTypeSrc = pack "f :: foo"
notDefinedTypeErrorMessage
  = "\"test.nb\":1:6-1:9\n\
    \ |\n\
    \ | f :: foo\n\
    \ |      ^^^\n\
    \name 'foo' is not defined"

incorrectConstArgumentTypeSrc = pack "a = \"nabla\" + \"delta\""
incorrectConstArgumentTypeErrorMessage
  = "\"test.nb\":1:5-1:11\n\
    \ |\n\
    \ | a = \"nabla\" + \"delta\"\n\
    \ |      ^^^^^^^^\n\
    \value '\"nabla\"' can NOT apply to (+)"

incorrectAliasArgumentTypeSrc = pack
  "f :: string -> int\n\
  \f a = a + 1\n\
  \"
incorrectAliasArgumentTypeErrorMessage
  = "\"test.nb\":1:5-1:11\n\
    \ |\n\
    \ | f a = a + 1\n\
    \ |       ^\n\
    \value 'a' can NOT apply to (+)"

incorrectFunctionTypeSrc = pack
  "a :: int\n\
  \a = \"str\"\n\
  \"
incorrectFunctionTypeErrorMessage
  = "\"test.nb\":1:5-1:9\n\
    \ |\n\
    \ | a = \"str\"\n\
    \ |      ^^^^^^\n\
    \value '\"str\"' can NOT apply to (+)"

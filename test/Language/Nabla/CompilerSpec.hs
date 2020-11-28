module Language.Nabla.CompilerSpec (spec) where

import Test.Hspec
import Data.Text

import Language.Nabla.Compiler

spec :: Spec
spec = do
  describe "Compile" $ do
    it "No Argument Function" $
      compile [("test.nb", noArgumentFunctionSrc)] `shouldBe` Right [("test.js", noArgumentFunctionDist)]
    it "Binary Operator Function" $
      compile [("test.nb", binaryOperatorFunctionSrc)] `shouldBe` Right [("test.js", binaryOperatorFunctionDist)]
    it "User-Defined Function" $
      compile [("test.nb", userDefinedFunctionSrc)] `shouldBe` Right [("test.js", userDefinedFunctionDist)]
    it "Higher Order Function" $
      compile [("test.nb", higherOrderFunctionSrc)] `shouldBe` Right [("test.js", higherOrderFunctionDist)]
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

noArgumentFunctionSrc = pack "a = 1"
noArgumentFunctionDist = pack "const a = () => 1;"

binaryOperatorFunctionSrc = pack "f a b = a + b"
binaryOperatorFunctionDist = pack "const f = (a) => (b) => a + b;"

userDefinedFunctionSrc
  = pack
  "id a = a\n\
  \g = id 2"
userDefinedFunctionDist
  = pack
  "const id = (a) => a;\n\
  \const g = () => id(2);"

higherOrderFunctionSrc
  = pack
  "apply f a = f a\n\
  \incr n = n + 1\n\
  \main = apply incr 2"
higherOrderFunctionDist
  = pack
  "const apply = (f) => (a) => f(a);\n\
  \const incr = (n) => n + 1;\n\
  \const main = () => apply(incr)(2);"

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

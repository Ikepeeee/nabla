{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module SbvTest where

import Data.SBV
import qualified Data.SBV.List as SL
import Control.Concurrent.Async

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#IntegerMinOrMax>
fastMinCorrect :: SInt32 -> SInt32 -> SBool
fastMinCorrect x y = m .== fm
  where m  = ite (x .< y) x y
        fm = y `xor` ((x `xor` y) .&. (-(oneIf (x .< y))));

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#IntegerMinOrMax>
fastMaxCorrect :: SInt32 -> SInt32 -> SBool
fastMaxCorrect x y = m .== fm
  where m  = ite (x .< y) y x
        fm = x `xor` ((x `xor` y) .&. (-(oneIf (x .< y))));

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#DetectOppositeSigns>
oppositeSignsCorrect :: SInt32 -> SInt32 -> SBool
oppositeSignsCorrect x y = r .== os
  where r  = (x .< 0 .&& y .>= 0) .|| (x .>= 0 .&& y .< 0)
        os = (x `xor` y) .< 0

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#ConditionalSetOrClearBitsWithoutBranching>
conditionalSetClearCorrect :: SBool -> SWord32 -> SWord32 -> SBool
conditionalSetClearCorrect f m w = r .== r'
  where r  = ite f (w .|. m) (w .&. complement m)
        r' = w `xor` ((-(oneIf f) `xor` w) .&. m);

-- | Formalizes <http://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2>
powerOfTwoCorrect :: SWord32 -> SBool
powerOfTwoCorrect v = f .== s
  where f = (v ./= 0) .&& ((v .&. (v-1)) .== 0);
        powers :: [Word32]
        powers = map ((2::Word32)^) [(0::Word32) .. 31]
        s = sAny (v .==) $ map literal powers

deMorgan :: SBool -> SBool -> SBool
deMorgan x y = (sNot x .&& sNot y) .== sNot (x .|| y)

bounding :: SBool -> SBool -> SBool -> SBool
bounding x y z = ((x .&& y) .|| z) .== (x .&& (y .|| z))

include' :: SInt32 -> SBool
include' x = ((x `sMod` 7) .== 0) .=> ((x `sMod` 2) .== 0)

gcd' :: SInt32 -> SInt32 -> SBool
gcd' x y = x .> 0 .&& y .> x .=> (x `sMod` y) .< y

fuzzy :: OrdSymbolic a => Num a => a -> SBool
fuzzy x = (0 .<= x) .&& (x .<= 1)

fAnd :: OrdSymbolic a => a -> a -> a
fAnd = smin

fOr :: OrdSymbolic a => a -> a -> a
fOr = smax

fNot :: Fractional a => a -> a
fNot x = 1.0 - x

fDeMorgan :: SDouble -> SDouble -> SBool
fDeMorgan x y = (fuzzy x .&& fuzzy y) .=> (fNot x `fAnd` fNot y) .== fNot (x `fOr` y)

fAnd' :: SDouble -> SDouble -> SDouble
fAnd' x y = x * y

fOr' :: Num a => a -> a -> a
fOr' x y = (x + y) - x * y

applyFOr' :: SDouble -> SDouble -> SBool
applyFOr' x y = (fuzzy x .&& fuzzy y .&& x .>= y) .=> fuzzy (x `fOr'` y)

sApplyFOr :: SReal -> SReal -> SBool
sApplyFOr x y = (fuzzy x .&& fuzzy y) .=> fuzzy (x `fOr'` y)

addMul :: SReal -> SReal -> SBool
addMul x y = (x .> 0 .&& y .> 0) .&& (x + y) .== (x * y)

listSize :: SList () -> SBool
listSize es = (SL.length es) - 1 .== (SL.length (SL.tail es))

n :: Integer
n = 0

addMul' :: IO AllSatResult
addMul' = allSat $ do
  [x,y] <- sIntegers ["x", "y"]
  solve [x .> (fromIntegral n), y .> 0, (x + y) .== 100]

triangle :: Symbolic SBool
triangle = do
  [a, b, c] <- sIntegers ["a", "b", "c"]
  return $ 100000 .< a -- .&& a .< 50
    .&& 100000 .< b -- .&& b .< 50
    .&& 0 .< c
    .&& a .>= b
    .&& (a * a + b * b .== c * c)

fermat :: Symbolic SBool
fermat = do
  [a, b, c] <- sIntegers ["a", "b", "c"]
  n <- sWord32 "n"
  return $ a .^ n .== 4

type' :: Symbolic SBool
type' = do
  [n] <- sIntegers ["n"]
  return $ n .>= 0 .&& n ./= 0 .=> n - 1 .> 0 .&& n - 2 .>= 0

-- | Collection of queries
queries :: IO ()
queries =
  let check :: Provable a => String -> a -> IO ()
      check w t = do putStr $ "Proving " ++ show w ++ ": "
                     print =<< prove t
  in do check "Fast min             " fastMinCorrect
        check "Fast max             " fastMaxCorrect
        check "Opposite signs       " oppositeSignsCorrect
        check "Conditional set/clear" conditionalSetClearCorrect
        check "PowerOfTwo           " powerOfTwoCorrect
        check "deMorgan             " deMorgan
        check "bounding             " bounding
        check "include              " include'
        check "gcd                  " gcd'
        --check "fDeMorgan            " fDeMorgan
        --check "x or y is fuzzy      " applyFOr
        check "x + y - xy = 0       " addMul

queries2 :: IO ()
queries2 = do
  results <- forConcurrently
    [fDeMorgan
    ,fDeMorgan
    ,fDeMorgan
    ,fDeMorgan
    ,fDeMorgan
    ,fDeMorgan
    ]
    prove
  print results

newtype SMTResult' = SMTResult' SMTResult

instance Show SMTResult' where
  show (SMTResult' (Satisfiable _ model)) = show model
  show _ = "other"

module Language.Nabla.Fixture where

import Data.SBV
import Data.SBV.RegExp
import Data.SBV.String as S

-- type FixtureSBVFun = (String, [String], String, [SData] -> SData)

data SData
  = SDBool SBool
  | SDDouble SDouble
  | SDString SString
  | SDRegex RegExp

data FixtureSBVFun =
  FixtureSBVFun
  { funName :: String
  , argTypes :: [String]
  , retType :: String
  , sbv :: [SData] -> SData
  }

fixtureFunTypes :: [FixtureSBVFun]
fixtureFunTypes =
  [ FixtureSBVFun "+" ["Double", "Double"] "Double" $ \[SDDouble a, SDDouble b] -> SDDouble (a + b)
  , FixtureSBVFun "+" ["String", "String"] "String" $ \[SDString a, SDString b] -> SDString (a S.++ b)
  , FixtureSBVFun "-" ["Double", "Double"] "Double" $ \[SDDouble a, SDDouble b] -> SDDouble (a - b)
  , FixtureSBVFun "*" ["Double", "Double"] "Double" $ \[SDDouble a, SDDouble b] -> SDDouble (a * b)
  , FixtureSBVFun "/" ["Double", "Double"] "Double" $ \[SDDouble a, SDDouble b] -> SDDouble (a / b)
  , FixtureSBVFun "&&" ["Bool", "Bool"] "Bool" $ \[SDBool a, SDBool b] -> SDBool (a .&& b)
  , FixtureSBVFun "||" ["Bool", "Bool"] "Bool" $ \[SDBool a, SDBool b] -> SDBool (a .|| b)
  , FixtureSBVFun "<=" ["Double", "Double"] "Bool" $ \[SDDouble a, SDDouble b] -> SDBool (a .<= b)
  , FixtureSBVFun "<" ["Double", "Double"] "Bool" $ \[SDDouble a, SDDouble b] -> SDBool (a .< b)
  , FixtureSBVFun ">=" ["Double", "Double"] "Bool" $ \[SDDouble a, SDDouble b] -> SDBool (a .>= b)
  , FixtureSBVFun ">" ["Double", "Double"] "Bool" $ \[SDDouble a, SDDouble b] -> SDBool (a .> b)
  , FixtureSBVFun "==" ["Double", "Double"] "Bool" $ \[SDDouble a, SDDouble b] -> SDBool (a .== b)
  , FixtureSBVFun "==" ["String", "String"] "Bool" $ \[SDString a, SDString b] -> SDBool (a .== b)
  , FixtureSBVFun "==" ["Bool", "Bool"] "Bool" $ \[SDBool a, SDBool b] -> SDBool (a .== b)
  , FixtureSBVFun "/=" ["Double", "Double"] "Bool" $ \[SDDouble a, SDDouble b] -> SDBool (a ./= b)
  , FixtureSBVFun "/=" ["String", "String"] "Bool" $ \[SDString a, SDString b] -> SDBool (a ./= b)
  , FixtureSBVFun "/=" ["Bool", "Bool"] "Bool" $ \[SDBool a, SDBool b] -> SDBool (a ./= b)
  , FixtureSBVFun "~=" ["String", "Regex"] "Bool"$ \[SDString a, SDRegex b] -> SDBool (a `match` b)
  , FixtureSBVFun "!" ["Bool"] "Bool" $ \[SDBool a] -> SDBool $ sNot a
  , FixtureSBVFun "-" ["Double"] "Double" $ \[SDDouble a] -> SDDouble $ - a
  ]

module Language.Nabla.SieveChecker where

import Language.Nabla.AST
import Language.Nabla.TypeChecker (inferValue, definedFun, definedArg, ArgType)
import Data.List as L
import Data.SBV
import Data.SBV.String as S
import Data.SBV.RegExp
import Data.Either (fromRight)
import Text.Megaparsec (parse)
import Language.Nabla.Regex (regex)
import Data.Functor (($>))
import Debug.Trace
import GHC.IO (unsafePerformIO)
import Control.Monad (zipWithM)

createSFunCond :: [(String, NFun)] -> NFun -> Symbolic SBool
createSFunCond funs fun = do
  let (NFun args body sieve) = fun
  let (NSieve sieveName sieveType sieveBody) = sieve
  let funTypes = map definedFun funs
  let argTypes = map definedArg args
  sArgs <- mapM createSArgVar args
  (sBody, funCallConds) <- toSData funs sArgs body
  argConds <- zipWithM createSieveCond (map snd sArgs) (map pickSieve args)
  bodyCond <- createSieveCond sBody sieve
  pure $ foldl' (.&&) sTrue (argConds <> funCallConds) .=> bodyCond

pickSieve :: NArg -> NSieve
pickSieve (NArg _ sieve) = sieve

createSieveCond :: SData -> NSieve -> Symbolic SBool
createSieveCond v (NSieve sieveName _ sieveBody) = do
  (SDBool c, _) <- toSData [] [(sieveName, v)] sieveBody
  pure c

createSArgVar :: NArg -> Symbolic (String, SData)
createSArgVar (NArg name (NSieve _ sieveType _)) = do
  v <- createTypeVar sieveType name
  pure (name, v)

createTypeVar :: String -> String -> Symbolic SData
createTypeVar typeName = do
  let sxVarCreator = lookup typeName sxVarCreators
  case sxVarCreator of
    (Just sxVarCreator) -> sxVarCreator
    Nothing -> error $ "not find type: " <> typeName

sxVarCreators :: [(String, String -> Symbolic SData)]
sxVarCreators =
  [ ("Double", fmap SDDouble . sDouble)
  , ("Bool", fmap SDBool . sBool)
  , ("String", fmap SDString . sString)
  ]

data SData
  = SDBool SBool
  | SDDouble SDouble
  | SDString SString
  | SDRegex RegExp

type SDataVar = (String, SData)

toArgType :: SDataVar -> ArgType
toArgType (name, SDBool _) = (name, "Bool")
toArgType (name, SDDouble _) = (name, "Double")
toArgType (name, SDString _) = (name, "String")
toArgType (name, SDRegex _) = (name, "Regex")

toSData :: [(String, NFun)] -> [SDataVar] -> NValue -> Symbolic (SData, [SBool])
toSData _ _ (NDouble v) = pure (SDDouble $ literal v, [])
toSData _ _ (NBool v) = pure (SDBool $ literal v, [])
toSData _ _ (NString v) = pure (SDString $ literal v, [])
toSData _ _ (NRegex v) = pure (SDRegex $ fromRight undefined (regex v), [])
toSData _ args (NVar name) = do
  let var = lookup name args
  case var of
    Just a -> pure (a, [])
    Nothing -> error $ "not find var in toSData" <> ": " <> name
toSData fs args (NIte c a b) = do
  (SDBool c', c'') <- toSData fs args c
  (SDDouble a', a'') <- toSData fs args a
  (SDDouble b', b'') <- toSData fs args b
  pure (SDDouble $ ite c' a' b', c'' <> a'' <> b'')
toSData funs args (NFixtureApp opName vs) = do
  let funTypes = map definedFun funs
  ts <- case mapM (inferValue funTypes (map toArgType args)) vs of
    Right ts -> pure ts
    Left e -> error e
  let op = find (\(name', argTypes, _, _) -> name' == opName && argTypes == ts) fixtureFunTypes
  case op of
    (Just (_, _, _, op)) -> do
      vs' <- mapM (toSData funs args) vs
      pure (op (map fst vs'), L.concatMap snd vs')
    Nothing -> error $ "not found op: " <> opName <> show ts
toSData fs args (NApp name _) = do
  (NFun _ _ (NSieve n t cond)) <- case lookup name fs of
    Just f -> pure f
    Nothing -> error $ "not find function: " <> name
  fnVar <- createTypeVar t n
  (SDBool cond', _) <- toSData fs [(n, fnVar)] cond
  pure (fnVar, [cond'])

type FixtureSBVFun = (String, [String], String, [SData] -> SData)

fixtureFunTypes :: [FixtureSBVFun]
fixtureFunTypes =
  [ ("+", ["Double", "Double"], "Double", \[SDDouble a, SDDouble b] -> SDDouble (a + b))
  , ("+", ["String", "String"], "String", \[SDString a, SDString b] -> SDString (a S.++ b))
  , ("-", ["Double", "Double"], "Double", \[SDDouble a, SDDouble b] -> SDDouble (a - b))
  , ("*", ["Double", "Double"], "Double", \[SDDouble a, SDDouble b] -> SDDouble (a * b))
  , ("/", ["Double", "Double"], "Double", \[SDDouble a, SDDouble b] -> SDDouble (a / b))
  , ("&&", ["Bool", "Bool"], "Bool", \[SDBool a, SDBool b] -> SDBool (a .&& b))
  , ("||", ["Bool", "Bool"], "Bool", \[SDBool a, SDBool b] -> SDBool (a .|| b))
  , ("<=", ["Double", "Double"], "Bool", \[SDDouble a, SDDouble b] -> SDBool (a .<= b))
  , ("<", ["Double", "Double"], "Bool", \[SDDouble a, SDDouble b] -> SDBool (a .< b))
  , (">=", ["Double", "Double"], "Bool", \[SDDouble a, SDDouble b] -> SDBool (a .>= b))
  , (">", ["Double", "Double"], "Bool", \[SDDouble a, SDDouble b] -> SDBool (a .> b))
  , ("==", ["Double", "Double"], "Bool", \[SDDouble a, SDDouble b] -> SDBool (a .== b))
  , ("==", ["String", "String"], "Bool", \[SDString a, SDString b] -> SDBool (a .== b))
  , ("==", ["Bool", "Bool"], "Bool", \[SDBool a, SDBool b] -> SDBool (a .== b))
  , ("/=", ["Double", "Double"], "Bool", \[SDDouble a, SDDouble b] -> SDBool (a ./= b))
  , ("/=", ["String", "String"], "Bool", \[SDString a, SDString b] -> SDBool (a ./= b))
  , ("/=", ["Bool", "Bool"], "Bool", \[SDBool a, SDBool b] -> SDBool (a ./= b))
  , ("~=", ["String", "Regex"], "Bool", \[SDString a, SDRegex b] -> SDBool (a `match` b))
  , ("!", ["Bool"], "Bool", \[SDBool a] -> SDBool $ sNot a)
  , ("-", ["Double"], "Double", \[SDDouble a] -> SDDouble $ - a)
  ]

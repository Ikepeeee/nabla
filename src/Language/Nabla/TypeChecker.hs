module Language.Nabla.TypeChecker where

import Language.Nabla.AST
import Data.List as L
import Data.SBV
import Data.SBV.String as S
import Data.SBV.RegExp
import Data.Either (fromRight)
import Text.Megaparsec (parse)
import Language.Nabla.Regex (regex)
import Debug.Trace
import GHC.IO (unsafePerformIO)

createCond :: [Fun] -> NFun -> Symbolic SBool
createCond funs (NFun args body (NSieve n typeName cond)) = do
  sArgs <- mapM (createSArg funs) args
  let sArgs' = map fst sArgs
  let actualRetType = infer funs sArgs' body
  actualRetTypeVar <- sString "actualRetType"
  expectedRetTypeVar <- sString "expectedRetType"
  (sBody, sCond1) <- toSData funs sArgs' body
  (SDBool sCond2, _) <- toSData funs [(n, sBody)] cond
  let sArgConds = map snd sArgs
  pure $ foldr (.&&) sTrue (sArgConds <> sCond1)
    .&& actualRetTypeVar .== literal actualRetType
    .&& expectedRetTypeVar .== literal typeName
    .=> actualRetTypeVar .== expectedRetTypeVar .&& sCond2

createSArg :: [Fun] -> NArg -> Symbolic ((String, SData), SBool)
createSArg fs (NArg name (NSieve n typeName cond)) = do
  v <- createType typeName name
  (SDBool c, _) <- toSData fs [(name, v)] cond
  pure ((name, v), c)

createType :: String -> String -> Symbolic SData
createType typeName = do
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

infer :: [Fun] -> [SDataVar] -> NValue -> String
infer _ _ (NDouble _) = "Double"
infer _ _ (NBool _) = "Bool"
infer _ _ (NString _) = "String"
infer _ _ (NRegex _) = "Regex"
infer _ args (NVar name) = do
  let sx = lookup name args
  case sx of
    Just (SDDouble _) -> "Double"
    Just (SDBool _) -> "Bool"
    Just (SDString _) -> "String"
    Just (SDRegex _) -> "Regex"
    Nothing -> error $ "not find var in infer" <> ": " <> name
infer _ _ NIte {} = "Double"
infer fs args (NFixtureApp opName vs) = do
  let ts = map (infer fs args) vs
  let op = lookup (opName:ts) functions
  case op of
    (Just (t, _)) -> t
    Nothing -> error $ "not found op: " <> opName <> show ts
infer fs args (NApp name vs) = do
  let f = lookup name fs
  case f of
    Just (NFun _ _ (NSieve _ t _)) -> t
    Nothing -> error $ "not found functions: " <> name

data SData
  = SDBool SBool
  | SDDouble SDouble
  | SDString SString
  | SDRegex RegExp

type Fun = (String, NFun)
type SDataVar = (String, SData)

toSData :: [Fun] -> [SDataVar] -> NValue -> Symbolic (SData, [SBool])
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
toSData fs args (NFixtureApp opName vs) = do
  let ts = map (infer fs args) vs
  let op = lookup (opName:ts) functions
  case op of
    (Just (_, op)) -> do
      vs' <- mapM (toSData fs args) vs
      pure (op (map fst vs'), L.concatMap snd vs')
    Nothing -> error $ "not found op: " <> opName <> show ts
toSData fs args (NApp name _) = do
  (NFun _ _ (NSieve n t cond)) <- case lookup name fs of
    Just f -> pure f
    Nothing -> error $ "not find function: " <> name
  fnVar <- createType t n
  (SDBool cond', _) <- toSData fs [(n, fnVar)] cond
  pure (fnVar, [cond'])

functions :: [([String], (String, [SData] -> SData))]
functions =
  [ (["+", "Double", "Double"], ("Double", \[SDDouble a, SDDouble b] -> SDDouble (a + b)))
  , (["+", "String", "String"], ("String", \[SDString a, SDString b] -> SDString (a S.++ b)))
  , (["-", "Double", "Double"], ("Double", \[SDDouble a, SDDouble b] -> SDDouble (a - b)))
  , (["*", "Double", "Double"], ("Double", \[SDDouble a, SDDouble b] -> SDDouble (a * b)))
  , (["/", "Double", "Double"], ("Double", \[SDDouble a, SDDouble b] -> SDDouble (a / b)))
  , (["&&", "Bool", "Bool"], ("Bool", \[SDBool a, SDBool b] -> SDBool (a .&& b)))
  , (["||", "Bool", "Bool"], ("Bool", \[SDBool a, SDBool b] -> SDBool (a .|| b)))
  , (["<=", "Double", "Double"], ("Bool", \[SDDouble a, SDDouble b] -> SDBool (a .<= b)))
  , (["<", "Double", "Double"], ("Bool", \[SDDouble a, SDDouble b] -> SDBool (a .< b)))
  , ([">=", "Double", "Double"], ("Bool", \[SDDouble a, SDDouble b] -> SDBool (a .>= b)))
  , ([">", "Double", "Double"], ("Bool", \[SDDouble a, SDDouble b]-> SDBool (a .> b)))
  , (["==", "Double", "Double"], ("Bool", \[SDDouble a, SDDouble b] -> SDBool (a .== b)))
  , (["==", "String", "String"], ("Bool", \[SDString a, SDString b] -> SDBool (a .== b)))
  , (["==", "Bool", "Bool"], ("Bool", \[SDBool a, SDBool b] -> SDBool (a .== b)))
  , (["/=", "Double", "Double"], ("Bool", \[SDDouble a, SDDouble b] -> SDBool (a ./= b)))
  , (["/=", "String", "String"], ("Bool", \[SDString a, SDString b] -> SDBool (a ./= b)))
  , (["/=", "Bool", "Bool"], ("Bool", \[SDBool a, SDBool b] -> SDBool (a ./= b)))
  , (["~=", "String", "Regex"], ("Bool", \[SDString a, SDRegex b] -> SDBool (a `match` b)))
  , (["!", "Bool"], ("Bool", \[SDBool a] -> SDBool $ sNot a))
  , (["-", "Double"], ("Double", \[SDDouble a] -> SDDouble $ - a))
  ]

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

createCond :: [(String, NFun)] -> NFun -> Symbolic SBool
createCond funs (NFun args body (NSieve n typeName cond)) = do
  sArgs <- mapM (createSArg funs) args
  let sArgs' = map fst sArgs
  (sBody, sCond1) <- toSData funs sArgs' body
  (SDBool sCond2, _) <- toSData funs [(n, sBody)] cond
  let sArgConds = map snd sArgs
  pure $ foldr (.&&) sTrue (sArgConds <> sCond1) .=> sCond2

createSArg :: [(String, NFun)] -> NArg -> Symbolic ((String, SData), SBool)
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

findVar :: String -> String -> [(String, SData)] -> SData
findVar message varName vars = do
  let var = lookup varName vars
  case var of
    (Just var) -> var
    Nothing -> error $ "not find var " <> message <> ": " <> varName

findFunc :: String -> [(String, NFun)] -> NFun
findFunc fnName fns = do
  let fn = lookup fnName fns
  case fn of
    (Just fn) -> fn
    Nothing -> error $ "not find function: " <> fnName

infer :: [(String, NFun)] -> [(String, SData)] -> NValue -> String
infer _ _ (NDouble _) = "Double"
infer _ _ (NBool _) = "Bool"
infer _ _ (NString _) = "String"
infer _ _ (NRegex _) = "Regex"
infer _ args (NVar name) = do
  let sx = findVar "infer" name args
  case sx of
    (SDDouble _) -> "Double"
    (SDBool _) -> "Bool"
    (SDString _) -> "String"
    (SDRegex _) -> "Regex"
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

toSData :: [(String, NFun)] -> [(String, SData)] -> NValue -> Symbolic (SData, [SBool])
toSData _ _ (NDouble v) = pure (SDDouble $ literal v, [])
toSData _ _ (NBool v) = pure (SDBool $ literal v, [])
toSData _ _ (NString v) = pure (SDString $ literal v, [])
toSData _ _ (NRegex v) = pure (SDRegex $ fromRight undefined (regex v), [])
toSData _ args (NVar name) = pure (findVar "arg" name args, [])
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
  let (NFun _ _ (NSieve n t cond)) = findFunc name fs
  fnVar <- createType t name
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

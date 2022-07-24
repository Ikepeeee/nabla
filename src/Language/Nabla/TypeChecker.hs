module Language.Nabla.TypeChecker where

import Language.Nabla.AST
import Data.SBV
import Data.SBV.String as S
import Data.SBV.RegExp
import Data.Either (fromRight)
import Text.Megaparsec (parse)
import Language.Nabla.Regex (regex)
import Debug.Trace
import GHC.IO (unsafePerformIO)

createCond :: [(String, NFunc)] -> NFunc -> Symbolic SBool
createCond fs (NFunc args body condArg _ cond) = do
  sArgs <- mapM (createSArg fs) args
  let sArgs' = map fst sArgs
  sBody <- _toSX fs sArgs' body
  (SXBool sCond) <- _toSX fs [(condArg, sBody)] cond
  let sArgConds = map snd sArgs
  pure $ foldr (.&&) sTrue sArgConds .=> sCond

createSArg :: [(String, NFunc)] -> NArg -> Symbolic ((String, SX), SBool)
createSArg fs (NArg name typeName cond) = do
  v <- createType typeName name
  (SXBool c) <- _toSX fs [(name, v)] cond
  pure ((name, v), c)

createType :: String -> String -> Symbolic SX
createType typeName = do
  let sxVarCreator = lookup typeName sxVarCreators
  case sxVarCreator of
    (Just sxVarCreator) -> sxVarCreator
    Nothing -> error $ "not find type: " <> typeName

sxVarCreators :: [(String, String -> Symbolic SX)]
sxVarCreators =
  [ ("Double", fmap SXDouble . sDouble)
  , ("Bool", fmap SXBool . sBool)
  , ("String", fmap SXString . sString)
  ]

findVar :: String -> String -> [(String, SX)] -> SX
findVar message varName vars = do
  let var = lookup varName vars
  case var of
    (Just var) -> var
    Nothing -> error $ "not find var " <> message <> ": " <> varName

findFunc :: String -> [(String, NFunc)] -> NFunc
findFunc fnName fns = do
  let fn = lookup fnName fns
  case fn of
    (Just fn) -> fn
    Nothing -> error $ "not find function: " <> fnName

infer :: [(String, NFunc)] -> [(String, SX)] -> NValue -> String
infer _ _ (NDouble _) = "Double"
infer _ _ (NBool _) = "Bool"
infer _ _ (NString _) = "String"
infer _ _ (NRegex _) = "Regex"
infer _ args (NVar name) = do
  let sx = findVar "infer" name args
  case sx of
    (SXDouble _) -> "Double"
    (SXBool _) -> "Bool"
    (SXString _) -> "String"
    (SXRegex _) -> "Regex"
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
    Just (NFunc _ _ _ t _) -> t
    Nothing -> error $ "not found functions: " <> name

data SX
  = SXBool SBool
  | SXDouble SDouble
  | SXString SString
  | SXRegex RegExp

_toSX :: [(String, NFunc)] -> [(String, SX)] -> NValue -> Symbolic SX
_toSX _ _ (NDouble v) = pure $ SXDouble $ literal  v
_toSX _ _ (NBool v) = pure $ SXBool $ literal v
_toSX _ _ (NString v) = pure $ SXString $ literal v
_toSX _ _ (NRegex v) = pure $ SXRegex $ fromRight undefined (regex v)
_toSX _ args (NVar name) = pure $ findVar "arg" name args
_toSX fs args (NIte c a b) = do
  (SXBool c') <- _toSX fs args c
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXDouble $ ite c' a' b'
_toSX fs args (NFixtureApp opName vs) = do
  let ts = map (infer fs args) vs
  let op = lookup (opName:ts) functions
  case op of
    (Just (_, op)) -> do
      vs' <- mapM (_toSX fs args) vs
      pure $ op vs'
    Nothing -> error $ "not found op: " <> opName <> show ts
-- TODO Fix here
_toSX fs args (NApp name vs) = do
  let (NFunc args' body _ _ _) = findFunc name fs
  vs' <- mapM (_toSX fs args) vs
  let argNames = map argName args'
  _toSX fs (args <> zip argNames vs') body

functions :: [([String], (String, [SX] -> SX))]
functions =
  [ (["+", "Double", "Double"], ("Double", \[SXDouble a, SXDouble b] -> SXDouble (a + b)))
  , (["+", "String", "String"], ("String", \[SXString a, SXString b] -> SXString (a S.++ b)))
  , (["-", "Double", "Double"], ("Double", \[SXDouble a, SXDouble b] -> SXDouble (a - b)))
  , (["*", "Double", "Double"], ("Double", \[SXDouble a, SXDouble b] -> SXDouble (a * b)))
  , (["/", "Double", "Double"], ("Double", \[SXDouble a, SXDouble b] -> SXDouble (a / b)))
  , (["&&", "Bool", "Bool"], ("Bool", \[SXBool a, SXBool b] -> SXBool (a .&& b)))
  , (["||", "Bool", "Bool"], ("Bool", \[SXBool a, SXBool b] -> SXBool (a .|| b)))
  , (["<=", "Double", "Double"], ("Bool", \[SXDouble a, SXDouble b] -> SXBool (a .<= b)))
  , (["<", "Double", "Double"], ("Bool", \[SXDouble a, SXDouble b] -> SXBool (a .< b)))
  , ([">=", "Double", "Double"], ("Bool", \[SXDouble a, SXDouble b] -> SXBool (a .>= b)))
  , ([">", "Double", "Double"], ("Bool", \[SXDouble a, SXDouble b]-> SXBool (a .> b)))
  , (["==", "Double", "Double"], ("Bool", \[SXDouble a, SXDouble b] -> SXBool (a .== b)))
  , (["==", "String", "String"], ("Bool", \[SXString a, SXString b] -> SXBool (a .== b)))
  , (["==", "Bool", "Bool"], ("Bool", \[SXBool a, SXBool b] -> SXBool (a .== b)))
  , (["/=", "Double", "Double"], ("Bool", \[SXDouble a, SXDouble b] -> SXBool (a ./= b)))
  , (["/=", "String", "String"], ("Bool", \[SXString a, SXString b] -> SXBool (a ./= b)))
  , (["/=", "Bool", "Bool"], ("Bool", \[SXBool a, SXBool b] -> SXBool (a ./= b)))
  , (["~=", "String", "Regex"], ("Bool", \[SXString a, SXRegex b] -> SXBool (a `match` b)))
  , (["!", "Bool"], ("Bool", \[SXBool a] -> SXBool $ sNot a))
  , (["-", "Double"], ("Double", \[SXDouble a] -> SXDouble $ - a))
  ]

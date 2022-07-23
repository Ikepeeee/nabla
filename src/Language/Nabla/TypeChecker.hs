module Language.Nabla.TypeChecker where

import Language.Nabla.AST
import Data.SBV
import Data.SBV.String as S
import Data.SBV.RegExp
import Data.SBV.Dynamic
import Data.Maybe
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

findVar :: String -> [(String, SX)] -> SX
findVar varName vars = do
  let var = lookup varName vars
  case var of
    (Just var) -> var
    Nothing -> error $ "not find var: " <> varName

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
  let sx = findVar name args
  case sx of
    (SXDouble _) -> "Double"
    (SXBool _) -> "Bool"
    (SXString _) -> "String"
    (SXRegex _) -> "Regex"
infer _ _ NIte {} = "Double"
infer fs args (NBin "+" a b) = do
  let aType = infer fs args a
  let bType = infer fs args b
  case [aType, bType] of
    ["Double", "Double"] -> "Double"
    ["String", "String"] -> "String"
    _ -> undefined
infer _ _ (NBin "-" _ _) = "Double"
infer _ _ (NBin "*" _ _) = "Double"
infer _ _ (NBin "/" _ _) = "Double"
infer _ _ (NBin ">=" _ _) = "Bool"
infer _ _ (NBin ">" _ _) = "Bool"
infer _ _ (NBin "<" _ _) = "Bool"
infer _ _ (NBin "<=" _ _) = "Bool"
infer _ _ (NBin "&&" _ _) = "Bool"
infer _ _ (NBin "||" _ _) = "Bool"
infer _ _ NBin {} = undefined
infer _ _ (NUni "!" _) = "Double"
infer _ _ (NUni "-" _) = "Double"
infer _ _ (NUni _ _) = undefined
infer fs args (NApp name _) = do
  let (NFunc _ v _ _ _) = findFunc name fs
  infer fs args v

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
_toSX _ args (NVar name) = pure $ findVar name args
_toSX fs args (NIte c a b) = do
  (SXBool c') <- _toSX fs args c
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXDouble $ ite c' a' b'
_toSX fs args (NBin opName a b) = do
  let aType = infer fs args a
  let bType = infer fs args b
  let op = lookup [opName, aType, bType] functions
  case op of
    (Just op) -> do
      a <- _toSX fs args a
      b <- _toSX fs args b
      pure $ op [a, b]
    Nothing -> error $ "not found op: " <> "+" <> aType <> " " <> bType
_toSX fs args (NUni opName a) = do
  let aType = infer fs args a
  let op = lookup [opName, aType] functions
  case op of
    (Just op) -> do
      a <- _toSX fs args a
      pure $ op [a]
    Nothing -> error $ "not found op: " <> "(+): " <> aType
-- TODO Fix here
_toSX fs args (NApp name vs) = do
  let (NFunc args' body _ _ _) = findFunc name fs
  vs' <- mapM (_toSX fs args) vs
  let argNames = map argName args'
  _toSX fs (args <> zip argNames vs') body

functions =
  [ (["+", "Double", "Double"], \[SXDouble a, SXDouble b] -> SXDouble (a + b))
  , (["+", "String", "String"], \[SXString a, SXString b] -> SXString (a S.++ b))
  , (["-", "Double", "Double"], \[SXDouble a, SXDouble b] -> SXDouble (a - b))
  , (["*", "Double", "Double"], \[SXDouble a, SXDouble b] -> SXDouble (a * b))
  , (["/", "Double", "Double"], \[SXDouble a, SXDouble b] -> SXDouble (a / b))
  , (["&&", "Bool", "Bool"], \[SXBool a, SXBool b] -> SXBool (a .&& b))
  , (["||", "Bool", "Bool"], \[SXBool a, SXBool b] -> SXBool (a .|| b))
  , (["<=", "Double", "Double"], \[SXDouble a, SXDouble b] -> SXBool (a .<= b))
  , (["<", "Double", "Double"], \[SXDouble a, SXDouble b] -> SXBool (a .< b))
  , ([">=", "Double", "Double"], \[SXDouble a, SXDouble b] -> SXBool (a .>= b))
  , ([">", "Double", "Double"], \[SXDouble a, SXDouble b]-> SXBool (a .> b))
  , (["==", "Double", "Double"], \[SXDouble a, SXDouble b] -> SXBool (a .== b))
  , (["==", "String", "String"], \[SXString a, SXString b] -> SXBool (a .== b))
  , (["==", "Bool", "Bool"], \[SXBool a, SXBool b] -> SXBool (a .== b))
  , (["/=", "Double", "Double"], \[SXDouble a, SXDouble b] -> SXBool (a ./= b))
  , (["/=", "String", "String"], \[SXString a, SXString b] -> SXBool (a ./= b))
  , (["/=", "Bool", "Bool"], \[SXBool a, SXBool b] -> SXBool (a ./= b))
  , (["/=", "Bool", "Bool"], \[SXBool a, SXBool b] -> SXBool (a ./= b))
  , (["~=", "String", "Regex"], \[SXString a, SXRegex b] -> SXBool (a `match` b))
  , (["!", "Bool"], \[SXBool a] -> SXBool $ sNot a)
  , (["-", "Double"], \[SXDouble a] -> SXDouble $ - a)
  ]

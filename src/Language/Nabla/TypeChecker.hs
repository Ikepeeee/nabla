module Language.Nabla.TypeChecker where

import Language.Nabla.AST
import Data.SBV
import Data.SBV.String as S
import Data.SBV.RegExp
import Data.Maybe
import Data.Either (fromRight)
import Text.Megaparsec (parse)
import Language.Nabla.Regex (regex)
import Debug.Trace

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
  v <- (fromJust $ lookup typeName sxVarCreators) name
  (SXBool c) <- _toSX fs [(name, v)] cond
  pure ((name, v), c)

sxVarCreators :: [([Char], String -> Symbolic SX)]
sxVarCreators =
  [ ("Double", fmap SXDouble . sDouble)
  , ("Bool", fmap SXBool . sBool)
  , ("String", fmap SXString . sString)
  ]

infer :: [(String, NFunc)] -> [(String, SX)] -> NValue -> String
infer _ _ (NDouble _) = "Double"
infer _ _ (NBool _) = "Bool"
infer _ _ (NString _) = "String"
infer _ _ (NRegex _) = "Regex"
infer _ args (NVar name) = do
  let sx = fromJust $ lookup (traceShow name name) (traceShow (map fst args) args)
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
  let (NFunc _ v _ _ _) = fromJust $ lookup name fs
  infer fs args v

data SX
  = SXBool SBool
  | SXDouble SDouble
  | SXString SString
  | SXRegex RegExp

_toSX :: [(String, NFunc)] -> [(String, SX)] -> NValue -> Symbolic SX
_toSX _ _ (NDouble v) = pure $ SXDouble $ literal v
_toSX _ _ (NBool v) = pure $ SXBool $ literal v
_toSX _ _ (NString v) = pure $ SXString $ literal v
_toSX _ _ (NRegex v) = pure $ SXRegex $ fromRight undefined (regex v)
_toSX _ args (NVar name) = pure $ fromJust $ lookup name args
_toSX fs args (NIte c a b) = do
  (SXBool c') <- _toSX fs args c
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXDouble $ ite c' a' b'
_toSX fs args (NBin "+" a b) = do
  let aType = infer fs args a
  let bType = infer fs args b
  case [aType, bType] of
    ["Double", "Double"] -> do
      (SXDouble a') <- _toSX fs args a
      (SXDouble b') <- _toSX fs args b
      pure $ SXDouble $ a' + b'
    ["String", "String"] -> do
      (SXString a') <- _toSX fs args a
      (SXString b') <- _toSX fs args b
      pure $ SXString $ a' S.++ b'
    _ -> error $ aType <> bType
_toSX fs args (NBin "-" a b) = do
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXDouble $ a' - b'
_toSX fs args (NBin "*" a b) = do
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXDouble $ a' * b'
_toSX fs args (NBin "/" a b) = do
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXDouble $ a' / b'
_toSX fs args (NBin "&&" a b) = do
  (SXBool a') <- _toSX fs args a
  (SXBool b') <- _toSX fs args b
  pure $ SXBool $ a' .&& b'
_toSX fs args (NBin "||" a b) = do
  (SXBool a') <- _toSX fs args a
  (SXBool b') <- _toSX fs args b
  pure $ SXBool $ a' .|| b'
_toSX fs args (NBin "<=" a b) = do
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXBool $ a' .<= b'
_toSX fs args (NBin "<" a b) = do
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXBool $ a' .< b'
_toSX fs args (NBin ">=" a b) = do
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXBool $ a' .>= b'
_toSX fs args (NBin ">" a b) = do
  (SXDouble a') <- _toSX fs args a
  (SXDouble b') <- _toSX fs args b
  pure $ SXBool $ a' .> b'
_toSX fs args (NBin "==" a b) = do
  let aType = infer fs args a
  let bType = infer fs args b
  case [aType, bType] of
    ["Double", "Double"] -> do
      (SXDouble a') <- _toSX fs args a
      (SXDouble b') <- _toSX fs args b
      pure $ SXBool $ a' .== b'
    ["Bool", "Bool"] -> do
      (SXBool a') <- _toSX fs args a
      (SXBool b') <- _toSX fs args b
      pure $ SXBool $ a' .== b'
    ["String", "String"] -> do
      (SXString a') <- _toSX fs args a
      (SXString b') <- _toSX fs args b
      pure $ SXBool $ a' .== b'
    _ -> undefined
_toSX fs args (NBin "/=" a b) = do
  let aType = infer fs args a
  let bType = infer fs args b
  case [aType, bType] of
    ["Double", "Double"] -> do
      (SXDouble a') <- _toSX fs args a
      (SXDouble b') <- _toSX fs args b
      pure $ SXBool $ a' ./= b'
    ["Bool", "Bool"] -> do
      (SXBool a') <- _toSX fs args a
      (SXBool b') <- _toSX fs args b
      pure $ SXBool $ a' ./= b'
    _ -> undefined
_toSX fs args (NBin "~=" a b) = do
  let aType = infer fs args a
  let bType = infer fs args b
  case [aType, bType] of
    ["String", "Regex"] -> do
      (SXString a') <- _toSX fs args a
      (SXRegex b') <- _toSX fs args b
      pure $ SXBool $ a' `match` b'
    _ -> undefined
_toSX _ _ NBin {} = undefined
_toSX fs args (NUni "!" v) = do
  (SXBool v') <- _toSX fs args v
  pure $ SXBool $ sNot v'
_toSX fs args (NUni "-" v) = do
  (SXDouble v') <- _toSX fs args v
  pure $ SXDouble $ - v'
_toSX _ _ (NUni _ _) = undefined
-- TODO Fix here
_toSX fs args (NApp name _) = do
  let (NFunc _ _ condArg _ cond) = fromJust $ lookup name fs
  _toSX fs args cond

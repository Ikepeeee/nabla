module Language.Nabla.TypeChecker where

import Language.Nabla.AST
import Data.SBV
import Data.SBV.String as S
import Data.SBV.RegExp
import Data.Maybe
import Data.Either (fromRight)
import Text.Megaparsec (parse)
import Language.Nabla.Regex (regex)

createCond :: NFunc -> Symbolic SBool
createCond (NFunc args body condArg _ cond) = do
  sArgs <- mapM createSArg args
  let sArgs' = map fst sArgs
  sBody <- _toSX sArgs' body
  (SXBool sCond) <- _toSX [(condArg, sBody)] cond
  let sArgConds = map snd sArgs
  pure $ foldr (.&&) sTrue sArgConds .=> sCond

createSArg :: NArg -> Symbolic ((String, SX), SBool)
createSArg (NArg name typeName cond) = do
  v <- (fromJust $ lookup typeName sxVarCreators) name
  (SXBool c) <- _toSX [(name, v)] cond
  pure ((name, v), c)

sxVarCreators :: [([Char], String -> Symbolic SX)]
sxVarCreators =
  [ ("Double", fmap SXDouble . sDouble)
  , ("Bool", fmap SXBool . sBool)
  , ("String", fmap SXString . sString)
  ]

infer :: [(String, SX)] -> NValue -> String
infer _ (NDouble _) = "Double"
infer _ (NBool _) = "Bool"
infer _ (NString _) = "String"
infer _ (NRegex _) = "Regex"
infer args (NVar name) = do
  let sx = fromJust $ lookup name args
  case sx of
    (SXDouble _) -> "Double"
    (SXBool _) -> "Bool"
    (SXString _) -> "String"
    (SXRegex _) -> "Regex"
infer _ NIte {} = "Double"
infer args (NBin "+" a b) = do
  let aType = infer args a
  let bType = infer args b
  case [aType, bType] of
    ["Double", "Double"] -> "Double"
    ["String", "String"] -> "String"
    _ -> undefined
infer _ (NBin "-" _ _) = "Double"
infer _ (NBin "*" _ _) = "Double"
infer _ (NBin "/" _ _) = "Double"
infer _ (NBin ">=" _ _) = "Bool"
infer _ (NBin ">" _ _) = "Bool"
infer _ (NBin "<" _ _) = "Bool"
infer _ (NBin "<=" _ _) = "Bool"
infer _ (NBin "&&" _ _) = "Bool"
infer _ (NBin "||" _ _) = "Bool"
infer _ NBin {} = undefined
infer _ (NUni "!" _) = "Double"
infer _ (NUni "-" _) = "Double"
infer _ (NUni _ _) = undefined

data SX
  = SXBool SBool
  | SXDouble SDouble
  | SXString SString
  | SXRegex RegExp

_toSX :: [(String, SX)] -> NValue -> Symbolic SX
_toSX _ (NDouble v) = pure $ SXDouble $ literal v
_toSX _ (NBool v) = pure $ SXBool $ literal v
_toSX _ (NString v) = pure $ SXString $ literal v
_toSX _ (NRegex v) = pure $ SXRegex $ fromRight undefined (regex v)
_toSX args (NVar name) = pure $ fromJust $ lookup name args
_toSX args (NIte c a b) = do
  (SXBool c') <- _toSX args c
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXDouble $ ite c' a' b'
_toSX args (NBin "+" a b) = do
  let aType = infer args a
  let bType = infer args b
  case [aType, bType] of
    ["Double", "Double"] -> do
      (SXDouble a') <- _toSX args a
      (SXDouble b') <- _toSX args b
      pure $ SXDouble $ a' + b'
    ["String", "String"] -> do
      (SXString a') <- _toSX args a
      (SXString b') <- _toSX args b
      pure $ SXString $ a' S.++ b'
    _ -> error $ aType <> bType
_toSX args (NBin "-" a b) = do
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXDouble $ a' - b'
_toSX args (NBin "*" a b) = do
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXDouble $ a' * b'
_toSX args (NBin "/" a b) = do
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXDouble $ a' / b'
_toSX args (NBin "&&" a b) = do
  (SXBool a') <- _toSX args a
  (SXBool b') <- _toSX args b
  pure $ SXBool $ a' .&& b'
_toSX args (NBin "||" a b) = do
  (SXBool a') <- _toSX args a
  (SXBool b') <- _toSX args b
  pure $ SXBool $ a' .|| b'
_toSX args (NBin "<=" a b) = do
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXBool $ a' .<= b'
_toSX args (NBin "<" a b) = do
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXBool $ a' .< b'
_toSX args (NBin ">=" a b) = do
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXBool $ a' .>= b'
_toSX args (NBin ">" a b) = do
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXBool $ a' .> b'
_toSX args (NBin "==" a b) = do
  let aType = infer args a
  let bType = infer args b
  case [aType, bType] of
    ["Double", "Double"] -> do
      (SXDouble a') <- _toSX args a
      (SXDouble b') <- _toSX args b
      pure $ SXBool $ a' .== b'
    ["Bool", "Bool"] -> do
      (SXBool a') <- _toSX args a
      (SXBool b') <- _toSX args b
      pure $ SXBool $ a' .== b'
    ["String", "String"] -> do
      (SXString a') <- _toSX args a
      (SXString b') <- _toSX args b
      pure $ SXBool $ a' .== b'
    _ -> undefined
_toSX args (NBin "/=" a b) = do
  let aType = infer args a
  let bType = infer args b
  case [aType, bType] of
    ["Double", "Double"] -> do
      (SXDouble a') <- _toSX args a
      (SXDouble b') <- _toSX args b
      pure $ SXBool $ a' ./= b'
    ["Bool", "Bool"] -> do
      (SXBool a') <- _toSX args a
      (SXBool b') <- _toSX args b
      pure $ SXBool $ a' ./= b'
    _ -> undefined
_toSX args (NBin "~=" a b) = do
  let aType = infer args a
  let bType = infer args b
  case [aType, bType] of
    ["String", "Regex"] -> do
      (SXString a') <- _toSX args a
      (SXRegex b') <- _toSX args b
      pure $ SXBool $ a' `match` b'
    _ -> undefined
_toSX _ NBin {} = undefined
_toSX args (NUni "!" v) = do
  (SXBool v') <- _toSX args v
  pure $ SXBool $ sNot v'
_toSX args (NUni "-" v) = do
  (SXDouble v') <- _toSX args v
  pure $ SXDouble $ - v'
_toSX _ (NUni _ _) = undefined

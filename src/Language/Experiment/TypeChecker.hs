module Language.Experiment.TypeChecker where

import Language.Experiment.AST
import Data.SBV
import Data.SBV.RegExp
import Data.Maybe

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
  case typeName of
    "Double" -> do
      v <- sDouble name
      (SXBool c) <- _toSX [(name, SXDouble v)] cond
      pure ((name, SXDouble v), c)
    "Bool" -> do
      v <- sBool name
      (SXBool c) <- _toSX [(name, SXBool v)] cond
      pure ((name, SXBool v), c)
    _ -> undefined

infer :: [(String, SX)] -> NValue -> String
infer _ (NDouble _) = "Double"
infer _ (NBool _) = "Bool"
infer args (NVar name) = do
  let sx = fromJust $ lookup name args
  case sx of
    (SXDouble _) -> "Double"
    (SXBool _) -> "Bool"
infer _ NIte {} = "Double"
infer _ (NBin "+" _ _) = "Double"
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


_toSX :: [(String, SX)] -> NValue -> Symbolic SX
_toSX _ (NDouble v) = pure $ SXDouble $ literal v
_toSX _ (NBool True) = pure $ SXBool sTrue
_toSX _ (NBool False) = pure $ SXBool sFalse
_toSX args (NVar name) = pure $ fromJust $ lookup name args
_toSX args (NIte c a b) = do
  (SXBool c') <- _toSX args c
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXDouble $ ite c' a' b'
_toSX args (NBin "+" a b) = do
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXDouble $ a' + b'
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
_toSX _ NBin {} = undefined
_toSX args (NUni "!" v) = do
  (SXBool v') <- _toSX args v
  pure $ SXBool $ sNot v'
_toSX args (NUni "-" v) = do
  (SXDouble v') <- _toSX args v
  pure $ SXDouble $ - v'
_toSX _ (NUni _ _) = undefined

module Language.Experiment.TypeChecker where

import Language.Experiment.AST
import Data.SBV
import Data.SBV.RegExp
import Data.Maybe

createCond :: NFunc -> Symbolic SBool
createCond (NFunc args body cond) = do
  sArgs <- mapM createSArg args
  let sArgs' = map fst sArgs
  (SXDouble sBody) <- _toSX sArgs' body
  (SXBool sCond) <- _toSX [("x", sBody)] cond
  let sArgConds = map snd sArgs
  pure $ foldr (.&&) sTrue sArgConds .=> sCond

createSArg :: NArg -> Symbolic ((String, SDouble), SBool)
createSArg (NArg name cond) = do
  v <- sDouble name
  (SXBool c) <- _toSX [(name, v)] cond
  pure ((name, v), c)

data SX
  = SXBool SBool
  | SXDouble SDouble


_toSX :: [(String, SDouble)] -> NValue -> Symbolic SX
_toSX _ (NDouble v) = pure $ SXDouble $ literal v
_toSX _ (NBool True) = pure $ SXBool sTrue
_toSX _ (NBool False) = pure $ SXBool sFalse
_toSX args (NDoubleVar name) = pure $ SXDouble $ fromJust $ lookup name args
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
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXBool $ a' .== b'
_toSX args (NBin "/=" a b) = do
  (SXDouble a') <- _toSX args a
  (SXDouble b') <- _toSX args b
  pure $ SXBool $ a' ./= b'
_toSX _ NBin {} = undefined
_toSX args (NUni "!" v) = do
  (SXBool v') <- _toSX args v
  pure $ SXBool $ sNot v'
_toSX _ (NUni _ _) = undefined

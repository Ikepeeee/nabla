module Language.Experiment.TypeChecker where

import Language.Experiment.AST
import Data.SBV
import Data.Maybe

createCond :: NFunc -> Symbolic SBool
createCond (NFunc args body cond) = do
  sArgs <- mapM createSArg args
  let sArgs' = map fst sArgs
  sBody <- _toSDouble sArgs' body
  sCond <- _toSBool [("x", sBody)] cond
  let sArgConds = map snd sArgs
  pure $ foldr (.&&) sTrue sArgConds .=> sCond

createSArg :: NArg -> Symbolic ((String, SDouble), SBool)
createSArg (NArg name cond) = do
  v <- sDouble name
  c <- _toSBool [(name, v)] cond
  pure ((name, v), c)

_toSBool :: [(String, SDouble)] -> NBool -> Symbolic SBool
_toSBool _ (NBool True) = pure sTrue
_toSBool _ (NBool False) = pure sFalse
_toSBool args (NAnd a b) = (.&&) <$> _toSBool args a <*> _toSBool args b
_toSBool args (NOr a b) = (.||) <$> _toSBool args a <*> _toSBool args b
_toSBool args (NNot a) = sNot <$> _toSBool args a
_toSBool args (NLe a b) = (.<=) <$> _toSDouble args a <*> _toSDouble args b
_toSBool args (NLt a b) = (.<) <$> _toSDouble args a <*> _toSDouble args b
_toSBool args (NGe a b) = (.>=) <$> _toSDouble args a <*> _toSDouble args b
_toSBool args (NGt a b) = (.>) <$> _toSDouble args a <*> _toSDouble args b
_toSBool args (NEq a b) = (.==) <$> _toSDouble args a <*> _toSDouble args b
_toSBool args (NNeq a b) = (./=) <$> _toSDouble args a <*> _toSDouble args b

_toSDouble :: [(String, SDouble)] -> NDouble -> Symbolic SDouble
_toSDouble _ (NDouble v) = pure $ literal v
_toSDouble args (NDoubleVar name) = pure $ fromJust $ lookup name args
_toSDouble args (NMinus a) = (* (-1.0)) <$> _toSDouble args a
_toSDouble args (NAdd a b) = (+) <$> _toSDouble args a <*> _toSDouble args b
_toSDouble args (NSub a b) = (-) <$> _toSDouble args a <*> _toSDouble args b
_toSDouble args (NMul a b) = (*) <$> _toSDouble args a <*> _toSDouble args b
_toSDouble args (NDiv a b) = (/) <$> _toSDouble args a <*> _toSDouble args b
_toSDouble args (NIte c a b) = ite <$> _toSBool args c <*> _toSDouble args a <*> _toSDouble args b

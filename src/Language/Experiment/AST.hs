module Language.Experiment.AST where

data NFunc = NFunc [NArg] NDouble NBool

data NArg = NArg String NBool

data NDouble
  = NDouble Double
  | NDoubleVar String
  | NMinus NDouble
  | NAdd NDouble NDouble
  | NSub NDouble NDouble
  | NMul NDouble NDouble
  | NDiv NDouble NDouble
  | NIte NBool NDouble NDouble

data NBool
  = NBool Bool
  | NAnd NBool NBool
  | NOr NBool NBool
  | NNot NBool
  | NLe NDouble NDouble
  | NLt NDouble NDouble
  | NGe NDouble NDouble
  | NGt NDouble NDouble
  | NEq NDouble NDouble
  | NNeq NDouble NDouble

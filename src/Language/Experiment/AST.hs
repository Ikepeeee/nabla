module Language.Experiment.AST where

data NFunc = NFunc [NArg] NValue String NValue

data NArg = NArg String NValue

data NValue
  = NDouble Double
  | NBool Bool
  | NDoubleVar String
  | NIte NValue NValue NValue
  | NBin String NValue NValue
  | NUni String NValue

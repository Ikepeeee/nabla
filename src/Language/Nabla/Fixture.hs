module Language.Nabla.Fixture where
import Data.Map (fromList, Map)
import Language.Nabla.AST

fixtureTypeEnv :: Map String Type
fixtureTypeEnv = fromList
  [ ("+", TFun TNum (TFun TNum TNum))
  ]

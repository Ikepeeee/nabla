module Language.Nabla.Fixture where
import Data.Map (fromList, Map)
import Language.Nabla.AST
import Language.Nabla.TypeChecker

fixtureTypeEnv :: Map String Type
fixtureTypeEnv = fromList
  [ ("+", TFun TNum (TFun TNum TNum))
  , ("-", TFun TNum (TFun TNum TNum))
  , ("*", TFun TNum (TFun TNum TNum))
  , ("/", TFun TNum (TFun TNum TNum))
  , (">", TFun TNum (TFun TNum TBool))
  , (">=", TFun TNum (TFun TNum TBool))
  , ("<", TFun TNum (TFun TNum TBool))
  , ("<=", TFun TNum (TFun TNum TBool))
  , ("==", TFun TNum (TFun TNum TBool))
  , ("&&", TFun TBool (TFun TBool TBool))
  , ("||", TFun TBool (TFun TBool TBool))
  , ("[unary]-", TFun TNum TNum)
  , ("[unary]+", TFun TNum TNum)
  , ("[unary]!", TFun TBool TBool)
  ]

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Nabla.Fixture where

import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Data.SBV
import Language.Nabla.AST

type EvaluatableFn = [String] -> String
type EvaluatableType = Const -> Bool
type EvaluatableFnType =　[AliasName]

fixtureFns :: [((Identifier p), EvaluatableFn)]
fixtureFns =
  [ (Fixture "+", \[a, b] -> a <> " + " <> b)
  , (Fixture "-", \[a, b] -> a <> " - " <> b)
  , (Fixture "*", \[a, b] -> a <> " * " <> b)
  , (Fixture "/", \[a, b] -> a <> " / " <> b)
  , (Fixture "and", \[a, b] -> a <> " && " <> b)
  , (Fixture "or", \[a, b] -> a <> " || " <> b)
  , (Fixture "not", \[a] -> "!" <> a)
  , (Fixture "p", \[a] -> "console.log(" <> a <> ")")
  ]

fixtureTypes :: [((Identifier p), EvaluatableType)]
fixtureTypes =
  [ (Fixture "Integer", tInteger)
  , (Fixture "Double", tDouble)
  , (Fixture "Bool", tBool)
  , (Fixture "String", tString)
  ]

fixtureFnTypes :: [((Identifier p), (FnType p))]
fixtureFnTypes =
  [ (Fixture "+", FnType [Type $ Fixture "Integer", Type $ Fixture "Integer"] (Type $ Fixture "Integer"))
  , (Fixture "+", FnType [Type $ Fixture "Double", Type $ Fixture "Double"] (Type $ Fixture "Double"))
  , (Fixture "-", FnType [Type $ Fixture "Integer", Type $ Fixture "Integer"] (Type $ Fixture "Integer"))
  , (Fixture "-", FnType [Type $ Fixture "Double", Type $ Fixture "Double"] (Type $ Fixture "Double"))
  , (Fixture "*", FnType [Type $ Fixture "Integer", Type $ Fixture "Integer"] (Type $ Fixture "Integer"))
  , (Fixture "*", FnType [Type $ Fixture "Double", Type $ Fixture "Double"] (Type $ Fixture "Double"))
  , (Fixture "/", FnType [Type $ Fixture "Integer", Type $ Fixture "Integer"] (Type $ Fixture "Integer"))
  , (Fixture "/", FnType [Type $ Fixture "Double", Type $ Fixture "Double"] (Type $ Fixture "Double"))
  , (Fixture "and", FnType [Type $ Fixture "Bool", Type $ Fixture "Bool"] (Type $ Fixture "Bool"))
  , (Fixture "or", FnType [Type $ Fixture "Bool", Type $ Fixture "Bool"] (Type $ Fixture "Bool"))
  , (Fixture "not", FnType [Type $ Fixture "Bool", Type $ Fixture "Bool"] (Type $ Fixture "Bool"))
  , (Fixture "p", FnType [Type $ Fixture "Any"] (Type $ Fixture "Void"))
  ]

tInteger :: EvaluatableType
tInteger (Direct c) = isJust (readMaybe c :: Maybe Integer)
tInteger _ = False

tDouble :: EvaluatableType
tDouble (Direct c) = isJust (readMaybe c :: Maybe Double)
tDouble _ = False

tBool :: EvaluatableType
tBool (Direct c) = isJust (readMaybe c :: Maybe Bool)
tBool _ = False

tString :: EvaluatableType
tString (Indirect _ ) = True
tString _ = False

direct :: Show a => a -> Const
direct = Direct . show

class Read' a where
  read' :: Const -> a

instance Read a => Read' a where
  read' (Direct c) = read c

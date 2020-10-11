{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Nabla.Fixture where

import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Data.SBV
import Language.Nabla.AST

type EvaluatableFn = [Const] -> Const
type EvaluatableType = Const -> Bool
type EvaluatableFnType =　[AliasName]

fixtureFns :: [((Identifier p), EvaluatableFn)]
fixtureFns =
  [ (Fixture "+", \[a, b] -> direct (read' a + read' b :: Integer))
  , (Fixture "+", \[a, b] -> direct (read' a + read' b :: Double))
  , (Fixture "-", \[a, b] -> direct (read' a - read' b :: Integer))
  , (Fixture "-", \[a, b] -> direct (read' a - read' b :: Double))
  , (Fixture "*", \[a, b] -> direct (read' a * read' b :: Integer))
  , (Fixture "*", \[a, b] -> direct (read' a * read' b :: Double))
  , (Fixture "/", \[a, b] -> direct (read' a `div` read' b :: Integer))
  , (Fixture "/", \[a, b] -> direct (read' a / read' b :: Double))
  , (Fixture "and", \[a, b] -> direct (read' a && read' b :: Bool))
  , (Fixture "or", \[a, b] -> direct (read' a || read' b :: Bool))
  , (Fixture "not", \[a] -> direct (not (read' a) :: Bool))
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

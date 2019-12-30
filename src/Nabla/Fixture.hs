module Nabla.Fixture where

import Nabla.IST

typeVars =
  [ ("string", string')
  , ("number", number')
  ]

string' :: Type
string' (StringV _) = True
string' _ = False

number' :: Type
number' (NumberV _) = True
number' _ = False

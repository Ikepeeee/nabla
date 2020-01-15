module Nabla.Fixture where

import Nabla.IST

typeVars =
  [ Type "string" string'
  , Type "number" number'
  ]

string' (StringV _) = True
string' _ = False

number' (NumberV _) = True
number' _ = False

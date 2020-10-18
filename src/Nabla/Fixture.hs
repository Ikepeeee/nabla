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

functionVars =
  [ ("add", add)
  ]

add :: Function
add (NumberV x) = FunctionV (\(NumberV y) -> NumberV $ show ((read x :: Float) + (read y :: Float)))

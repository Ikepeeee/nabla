module Nabla.Fixture where

import Nabla.AST

functions =
  [ ("add", nAdd)
  -- , ("sub", (-))
  -- , ("div", (/))
  -- , ("mul", (*))
  ]

nAdd :: [String] -> String
nAdd (a:b:_) = show $ (read a) + (read b)

module Nabla.Executor where

import Nabla.AST

exec :: AST -> IO (Either String ())
exec (AST []) = pure $ Right ()
exec (AST (expr:exprs)) = do
  print expr
  exec $ AST exprs

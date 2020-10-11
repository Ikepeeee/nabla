module Language.Nabla.Interpreter where

import Text.Read
import qualified Data.Map as Map
import Language.Nabla.AST
import Language.Nabla.Fixture

-- evaluate :: App -> [(Identifier, Value)] -> Maybe Value
-- evaluate (App vs) cs = simplify vs cs
-- evaluate _ _ = Nothing

-- -- Simplify value
-- simplify :: Value -> [(Identifier, Value)] -> Maybe Value
-- simplify [(Const c)] _ = pure $ Const c
-- simplify ((Alias name):vs) cs
--   | elem name (map fst cs) = lookup name cs
--   | Map.member name embeddedFns = (Map.lookup name embeddedFns) >>= (\f -> f <$> args) >>= pure . Const
--   | otherwise = Nothing
--   where
--     args = mapM (\f -> evaluate f cs) vs
-- simplify ((FnValue (Fn _ fnBody)):args) cs = (Map.lookup name embeddedFns) >>= (\f -> f <$> args)
-- simplify
-- simplify _ _ = Nothing

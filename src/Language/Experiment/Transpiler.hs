module Language.Experiment.Transpiler where

import Data.List
import Language.Experiment.AST

-- transFunc :: NFunc -> String
-- transFunc (NFunc args body _) = "(" <> intercalate ", " (map transArg args) <> ")" <> " => " <> transNDouble body

-- transArg :: NArg -> String
-- transArg (NArg name _) = name

-- transNDouble :: NDouble -> String
-- transNDouble (NDouble v) = show v
-- transNDouble (NDoubleVar name) = name
-- transNDouble (NMinus v) = "- (" <> transNDouble v <> ")"
-- transNDouble (NAdd a b) = "(" <> transNDouble a <> " + " <> transNDouble b <> ")"
-- transNDouble (NSub a b) = "(" <> transNDouble a <> " - " <> transNDouble b <> ")"
-- transNDouble (NMul a b) = "(" <> transNDouble a <> " * " <> transNDouble b <> ")"
-- transNDouble (NDiv a b) = "(" <> transNDouble a <> " / " <> transNDouble b <> ")"
-- transNDouble (NIte c a b) = "(" <> transNBool c <> " ? " <> transNDouble a <> " : " <> transNDouble b <> ")"


-- transNBool :: NBool -> String
-- transNBool (NBool True) = "true"
-- transNBool (NBool False) = "false"
-- transNBool (NAnd a b) = "(" <> transNBool a <> " && " <> transNBool b <> ")"
-- transNBool (NOr a b) = "(" <> transNBool a <> " || " <> transNBool b <> ")"
-- transNBool (NNot a) = "!" <> transNBool a
-- transNBool (NLe a b) = "(" <> transNDouble a <> "<=" <> transNDouble b <> ")"
-- transNBool (NLt a b) = "(" <> transNDouble a <> "<" <> transNDouble b <> ")"
-- transNBool (NGe a b) = "(" <> transNDouble a <> ">=" <> transNDouble b <> ")"
-- transNBool (NGt a b) = "(" <> transNDouble a <> ">" <> transNDouble b <> ")"
-- transNBool (NEq a b) = "(" <> transNDouble a <> "==" <> transNDouble b <> ")"
-- transNBool (NNeq a b) = "(" <> transNDouble a <> "!=" <> transNDouble b <> ")"

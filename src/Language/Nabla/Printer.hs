module Language.Nabla.Printer where

import Data.List
import Language.Nabla.AST

transFunc :: NFun -> String
transFunc (NFun args body _) = "(" <> intercalate ", " (map transArg args) <> ")" <> " => " <> transNValue body

transArg :: NArg -> String
transArg (NArg name _) = name

transNValue :: NValue -> String
transNValue (NDouble v) = show v
transNValue (NBool True) = "true"
transNValue (NBool False) = "false"
transNValue (NString v) = "'" <> v <> "'"
transNValue (NRegex v) = "/" <> v <> "/i"
transNValue (NVar v) = v
transNValue (NIte c a b) = "(" <> transNValue c <> " ? " <> transNValue a <> " : " <> transNValue b <> ")"
transNValue (NApp op [a, b]) = "(" <> transNValue a <> " " <> op <> " " <> transNValue b <> ")"
transNValue (NApp op [v]) = "(" <> op <> " " <> transNValue v <> ")"
transNValue (NApp op _) = undefined

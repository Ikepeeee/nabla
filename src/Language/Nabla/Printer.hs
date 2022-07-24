module Language.Nabla.Printer where

import Data.List
import Language.Nabla.AST

transFunc :: NFunc -> String
transFunc (NFunc args body _ _ _) = "(" <> intercalate ", " (map transArg args) <> ")" <> " => " <> transNValue body

transArg :: NArg -> String
transArg (NArg name _ _) = name

transNValue :: NValue -> String
transNValue (NDouble v) = show v
transNValue (NBool True) = "true"
transNValue (NBool False) = "false"
transNValue (NString v) = "'" <> v <> "'"
transNValue (NRegex v) = "/" <> v <> "/i"
transNValue (NVar v) = v
transNValue (NIte c a b) = "(" <> transNValue c <> " ? " <> transNValue a <> " : " <> transNValue b <> ")"
transNValue (NFixtureApp op [a, b]) = "(" <> transNValue a <> " " <> op <> " " <> transNValue b <> ")"
transNValue (NFixtureApp op [v]) = "(" <> op <> " " <> transNValue v <> ")"
transNValue (NFixtureApp op _) = undefined

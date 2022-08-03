module Language.Nabla.TypeChecker where

import Data.List ( find )
import Language.Nabla.AST
    ( NValue(..), NArg(..), NSieve(NSieve), NFun(..) )
import Control.Monad (when)
import Data.Functor (($>))
import Debug.Trace

show' :: Show a => a -> a
show' a = traceShow a a

validFuns :: [(String, NFun)] -> Either String ()
validFuns funs = mapM (validFun funs) funs $> ()

validFun :: [(String, NFun)] -> (String, NFun) -> Either String FunType
validFun funs (name, fun) = do
  let (NFun args body sieve) = fun
  let (NSieve sieveName sieveType sieveBody) = sieve
  let funTypes = map definedFun funs
  let argTypes = map definedArg args
  let (_, argType, retType) = definedFun (name, fun)
  mapM_ validArg args
  inferredSieveBodyType <- inferValue [] [(sieveName, sieveType)] sieveBody
  inferredRetType <- inferValue funTypes argTypes body
  when (inferredRetType /= retType)
    (Left $ "function " <> name <> " is not returned " <> retType)
  when (inferredSieveBodyType /= "Bool")
    (Left $ "function " <> name <> " sieve is not returned Bool")
  pure (name, argType, retType)

validArg :: NArg -> Either String ()
validArg (NArg name (NSieve sieveName sieveType sieveBody)) = do
  inferredSieveBodyType <- inferValue [] [(sieveName, sieveType)] sieveBody
  when (inferredSieveBodyType /= "Bool")
    (Left $ "argument " <> name <> " sieve is not returned Bool")

definedFun :: (String, NFun) -> FunType
definedFun (name, NFun args _ (NSieve _ retType _))
  = (name, map (snd . definedArg) args, retType)

definedArg :: NArg -> ArgType
definedArg (NArg name (NSieve _ t _)) = (name, t)

type ArgType = (String, String)
type FunType = (String, [String], String)

inferValue :: [FunType] -> [ArgType] -> NValue -> Either String String
inferValue _ _ (NDouble _) = pure "Double"
inferValue _ _ (NBool _) = pure "Bool"
inferValue _ _ (NString _) = pure "String"
inferValue _ _ (NRegex _) = pure "Regex"
inferValue _ args (NVar name) = lookup' ("not found arg: " <> name) name args
inferValue funs args (NIte c a b) = do
  cType <- inferValue funs args c
  aType <- inferValue funs args a
  bType <- inferValue funs args b
  when (cType /= "Bool") (Left "if condition value should be Bool")
  when (aType /= bType) (Left "then value and else value should be same type")
  pure aType
inferValue funs args (NFixtureApp name vs) = do
  vTypes <- mapM (inferValue funs args) vs
  (_, _, retType) <- find' ("not found fixture function: " <> name <> " with argument type " <> show vTypes)
    (\(name', argTypes, _) -> name' == name && argTypes == vTypes)
    fixtureFunTypes
  pure retType
inferValue funs args (NApp name vs) = do
  vTypes <- mapM (inferValue funs args) vs
  (_, _, retType) <- find' ("not found function: " <> name <> " with argument type " <> show vTypes)
    (\(name', argTypes, _) -> name' == name && argTypes == vTypes)
    funs
  pure retType

find' :: e -> (a -> Bool) -> [a] -> Either e a
find' e a vs = case find a vs of
  Just a -> Right a
  Nothing -> Left e

lookup' :: Eq a => e -> a -> [(a, b)] -> Either e b
lookup' e a vs = case lookup a vs of
  Just b -> Right b
  Nothing -> Left e

fixtureFunTypes :: [FunType]
fixtureFunTypes =
  [ ("+", ["Double", "Double"], "Double")
  , ("+", ["String", "String"], "String")
  , ("-", ["Double", "Double"], "Double")
  , ("*", ["Double", "Double"], "Double")
  , ("/", ["Double", "Double"], "Double")
  , ("&&", ["Bool", "Bool"], "Bool")
  , ("||", ["Bool", "Bool"], "Bool")
  , ("<=", ["Double", "Double"], "Bool")
  , ("<", ["Double", "Double"], "Bool")
  , (">=", ["Double", "Double"], "Bool")
  , (">", ["Double", "Double"], "Bool")
  , ("==", ["Double", "Double"], "Bool")
  , ("==", ["String", "String"], "Bool")
  , ("==", ["Bool", "Bool"], "Bool")
  , ("/=", ["Double", "Double"], "Bool")
  , ("/=", ["String", "String"], "Bool")
  , ("/=", ["Bool", "Bool"], "Bool")
  , ("~=", ["String", "Regex"], "Bool")
  , ("!", ["Bool"], "Bool")
  , ("-", ["Double"], "Double")
  ]

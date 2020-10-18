module Main where

import Lib
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

main :: IO ()
main = do
  result <- (`evalStateT` 0) $ runExceptT $ do
    i <- lift $ get
    return ()
  case result of
    Right _ -> return ()

func :: StateT String Identity String
func = return ""

func' :: StateT String IO String
func' = return ""

func'' :: StateT String Maybe String
func'' = return ""

func3 :: String
func3 = evalState func ""

func'3 :: IO String
func'3 = evalStateT func' ""

func''3 :: Maybe String
func''3 = (`evalStateT` "") func''

func2 :: String
func2 = runIdentity $ do
  return ""


{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as DTI
import Language.Nabla.Compiler (compile)

main :: IO ()
main = do
  [fileName] <- getArgs
  src <- DTI.readFile fileName
  case compile [(fileName, src)] of
    Right [(_, dist)] -> print dist
    Left e -> putStrLn e
  return ()

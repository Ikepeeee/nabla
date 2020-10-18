-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecursiveDo #-}

module Compile where

-- import Data.Text.Lazy.IO as T

-- import LLVM.Pretty  -- from the llvm-hs-pretty package
-- import LLVM.AST hiding (function, value)
-- import LLVM.AST.Type as AST
-- import qualified LLVM.AST.Float as F
-- import qualified LLVM.AST.Constant as C
-- import qualified LLVM.AST.Operand as O

-- import LLVM.IRBuilder.Module
-- import LLVM.IRBuilder.Monad
-- import LLVM.IRBuilder.Instruction
-- import LLVM.IRBuilder.Constant

-- main :: IO ()
-- main = T.writeFile "./build/main.ll" $ ppllvm $ buildModule "exampleModule" $ mdo
--   add' <- function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
--     entry <- block `named` "entry"; mdo
--       c <- add a b
--       ret c
--   -- fibo <- function "fibo" [(i32, "n")] i32 $ \[n] -> mdo
--   --   entry <- block `named` "entry"; mdo
--   --     incomingValues Instruction
--   function "main" [] i32 $ \[] -> mdo
--     entry <- block `named` "entry"; mdo
--       let a = int32 1
--       let b = int32 3
--       c <- call add' [(a, []), (b, [])]
--       ret c

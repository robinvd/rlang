module Rlang.Jit where

import Data.Int
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> (IO Int)

run :: FunPtr a -> IO Int
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 3  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultPassSetSpec
  { transforms = 
    [ 
    AlwaysInline True
    -- , FunctionInlining 10
    -- , PromoteMemoryToRegister
    -- , InstructionCombining
    -- , ConstantPropagation
    -- , DeadCodeElimination
    -- , AggressiveDeadCodeElimination
    -- , GlobalDeadCodeElimination
    -- , FunctionAttributes
    -- , TailCallElimination
    ]
  }

runJIT :: AST.Module -> IO (AST.Module)
runJIT mod = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      withModuleFromAST context mod $ \m -> do
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          -- runPassManager pm m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          B.putStrLn s
          B.writeFile "out.ll" s
          verify m

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.mkName "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> putStrLn "error" >> return ()

          -- Return the optimized module
          return optmod

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rlang.Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Rlang.Codegen
import qualified Rlang.Core as S
import qualified Rlang.Syntax as S

typeToLLVM :: S.Type -> Type
typeToLLVM t = case t of
                 S.TType "Char" -> IntegerType 8
                 S.TType "Num" -> IntegerType 64
                 S.TUnit -> IntegerType 1
                 S.TFunc ret args -> 
                   FunctionType (typeToLLVM ret) (map typeToLLVM args) False

codegenTop :: S.CFunc -> LLVM ()
codegenTop S.CFunc{..} =
  define (typeToLLVM retType) (T.unpack name) fnargs $ bls
  where
    fnargs = toSig $ map fst args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca int
        store var (local (AST.mkName (T.unpack (fst a))))
        assign (fst a) var
      cgen body >>= ret

prelude :: LLVM ()
prelude = do
  let args = ["a","b"]
  defineInline int "+" (toSig args) $
    createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      res <- (instr $ Add False False
        (LocalReference int (Name "a"))
        (LocalReference int (Name "b")) [])
      ret res
  defineInline int "-" (toSig args) $
    createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      res <- (instr $ Sub False False
        (LocalReference int (Name "a"))
        (LocalReference int (Name "b")) [])
      ret res

toSig :: [Text] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (int, AST.mkName (T.unpack x)))

cgen :: S.CBlock -> Codegen AST.Operand
cgen S.CBlock {..} = last <$> mapM gen blockBody
  where
    gen :: S.CExpr -> Codegen AST.Operand
    gen expr = case expr of
                 S.CCall fn args -> do
                   largs <- mapM cgen args
                   call (externf (AST.mkName (T.unpack fn))) largs
                 (S.CAssign var st) -> do
                   ptr <- getvar var
                   res <- cgen st
                   store ptr res
                 (S.CLit pr) -> primCgen pr
                 (S.CVar name) -> getvar name >>= load
                 (S.CInit name t) -> do
                   var <- alloca int
                   assign name var
                   return var
                 -- TODO actually make a new scope
                 S.CScope bl -> cgen bl
                 S.CIf cond tr fl -> do
                   ifthen <- addBlock "if.then"
                   ifelse <- addBlock "if.else"
                   ifexit <- addBlock "if.exit"

                   -- %entry
                   ------------------
                   cond <- cgen cond
                   test <- icmp IP.EQ (cons $ C.Int 64 0) cond
                   cbr test ifthen ifelse -- Branch based on the condition

                   -- if.then
                   ------------------
                   setBlock ifthen
                   trval <- cgen tr       -- Generate code for the true branch
                   br ifexit              -- Branch to the merge block
                   ifthen <- getBlock

                   -- if.else
                   ------------------
                   setBlock ifelse
                   flval <- cgen fl       -- Generate code for the false branch
                   br ifexit              -- Branch to the merge block
                   ifelse <- getBlock

                   -- if.exit
                   ------------------
                   setBlock ifexit
                   phi int [(trval, ifthen), (flval, ifelse)]

                 S.CWhile _ _ -> error "no while yet"
                 -- (S.Ret x) -> do
                 --   s <- stCgen x
                 --   ret s
                 --   return s

primCgen :: S.Prim -> Codegen AST.Operand
-- primCGen (String str) =
primCgen (S.Char ch) = return $ cons $ C.Int 8 (toInteger $ fromEnum ch)
primCgen (S.Num i) = return $ cons $ C.Int 64 (toInteger i)
-- primCGen (Tuple)
primCgen (S.Unit) = primCgen (S.Num 0)

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.CFunc] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    -- putStrLn llstr
    return newast
  where
    modn = mapM codegenTop fns >> prelude
    newast = runLLVM mod modn

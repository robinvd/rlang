{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rlang.Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import LLVM.AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

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

codegenTop :: S.CFunc -> LLVM ()
codegenTop S.CFunc{..} =
  define int (T.unpack name) fnargs $ bls
  where
    fnargs = toSig $ map fst args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca int
        store var (local (AST.mkName (T.unpack (fst a))))
        assign (fst a) var
      mapM cgen body -- >>= ret

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

cgen :: S.CExpr -> Codegen AST.Operand
cgen (S.CStatement st) = stCgen st
cgen (S.CWhile st body) = undefined
cgen (S.CIf cond t f) = undefined
cgen (S.CScope body) = mapM cgen body >>= return . head

stCgen :: S.CStatement -> Codegen AST.Operand
stCgen (S.CCall fn args) = do
  largs <- mapM stCgen args
  call (externf (AST.mkName (T.unpack fn))) largs
stCgen (S.CAssign var st) = do
  ptr <- getvar var
  res <- stCgen st
  store ptr res
stCgen (S.CLit pr) = primCgen pr
stCgen (S.CVar name) = getvar name >>= load
stCgen (S.CInit name t) = do
  var <- alloca int
  assign name var
  return var
stCgen (S.Ret x) = do
  s <- stCgen x
  ret s
  return s

primCgen :: S.Prim -> Codegen AST.Operand
-- primCGen (String str) =
primCgen (S.Char ch) = return $ cons $ C.Int 8 (toInteger $ fromEnum ch)
primCgen (S.Num i) = return $ cons $ C.Int 64 (toInteger i)
-- primCGen (Tuple)
-- primCGen (Unit)

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

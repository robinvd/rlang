{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rlang.Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import LLVM.AST
import qualified LLVM.AST.Type as T
import LLVM.AST.Global
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Int
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (gets)
import Control.Applicative
import qualified Data.Map as Map

import Rlang.Codegen
import Rlang.Scan (Env)
import qualified Rlang.Core as S
import Rlang.Core (typeToLLVM)
import qualified Rlang.Syntax as S
import qualified Rlang.MapStack as MS


codegenTop :: Env -> S.CFunc -> LLVM ()
codegenTop env S.CFunc{..} =
  define (typeToLLVM retType) (T.unpack name) fnargs $ bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen (fmap typeToLLVM env) $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca (typeToLLVM (snd a))
        store var (local (AST.mkName (T.unpack (fst a))))
        assign (fst a) var
      cgen (M.fromList args) body >>= ret

prelude :: LLVM ()
prelude = do
  let args = [("a", S.TType "Num" []),("b",S.TType "Num" [])]
  defineInline int "+" (toSig args) $
    createBlocks $ execCodegen M.empty $ do
      entry <- addBlock entryBlockName
      setBlock entry
      res <- (instr $ Add False False
        (LocalReference int (Name "a"))
        (LocalReference int (Name "b")) [])
      ret res
  defineInline int "-" (toSig args) $
    createBlocks $ execCodegen M.empty $ do
      entry <- addBlock entryBlockName
      setBlock entry
      res <- (instr $ Sub False False
        (LocalReference int (Name "a"))
        (LocalReference int (Name "b")) [])
      ret res
  let c = typeToLLVM (S.TType "Char" [])
  let n = typeToLLVM (S.TType "Num" [])
  external VoidType "putchar" [(c, mkName "in" )]
  external (T.ptr (StructureType False [int, int])) "malloc" [(n, Name "size" )]
  -- define (typeToLLVM (S.TUnit)) "put2" [(c, mkName "in")] $
  --   createBlocks $ execCodegen M.empty $ do
  --     entry <- addBlock entryBlockName
  --     setBlock entry
  --     ret void

toSig :: [(Text, S.Type)] -> [(AST.Type, AST.Name)]
toSig = map (\(x,t) -> (typeToLLVM t, AST.mkName (T.unpack x)))

cgen :: S.CBlock -> Codegen AST.Operand
cgen envv S.CBlock {..} = last <$> mapM (gen (envv)) blockBody
  where
    isLocal :: Env -> Text -> Bool
    isLocal env name = case M.lookup name env of
                         Just _ -> True
                         _ -> False
    gen :: S.CExpr -> Codegen AST.Operand
    gen local expr = case expr of
                 S.CCall fn args -> do
                   largs <- mapM (cgen local) args
                   case isLocal local fn of
                     True -> do
                       f <- getvar fn >>= load
                       call f largs
                       -- return f
                     False -> call (externf (AST.mkName (T.unpack fn))) largs
                 (S.CAssign var st) -> do
                   ptr <- getvar var
                   res <- cgen st
                   store ptr res
                 (S.CLit pr) -> primCgen pr
                 S.CStruct name t fields -> do
                   global <- gets globalTable
                   ptr <- alloca t
                   parts <- mapM (cgen ) fields
                   forM (zip [0..] parts) $ \(i, v) -> do
                     indexPtr <- instr $ GetElementPtr True ptr [cons (C.Int 64 i)] []
                     store indexPtr v
                   return $ ptr
                 (S.CVar name) -> getvar name >>= load
                 (S.CInit name t) -> do
                   var <- alloca (typeToLLVM t)
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
primCgen (S.Unit) = return $ cons $ C.Int 1 0
-- primCGen env (S.Tulple xs) = do
  -- parts <- mapM (cgen env) xs
  -- return $ C.Struct Nothing True parts

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: Env -> AST.Module -> [S.CFunc] -> IO AST.Module
codegen env mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    -- putStrLn llstr
    return newast
  where
    modn = mapM (codegenTop env) fns >> prelude
    newast = runLLVM mod modn

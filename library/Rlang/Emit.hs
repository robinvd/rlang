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
import qualified LLVM.AST.Visibility as V

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Data.Int
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (gets, modify)
import Control.Applicative
import qualified Data.Map as Map

import Rlang.Codegen
import Rlang.Scan (Env)
import qualified Rlang.Core as S
import Rlang.Core (typeToLLVM)
import qualified Rlang.Syntax as S
import qualified Rlang.MapStack as MS

import Text.Pretty.Simple (pPrint)


codegenTop :: Env -> S.CFunc -> LLVM ()
codegenTop env S.CFunc{..} = do
  addDefn $ GlobalDefinition $ functionDefaults
    { name = makeName name
    , parameters = mkParameters fnargs
    , returnType = typeToLLVM retType
    , basicBlocks = bls
    , visibility = if public then V.Default else V.Hidden}
  mapM_ (addDefn . GlobalDefinition . snd) (M.toList $ toGlobal st)
  where
    fnargs = toSig args
    globalToOperand :: Text -> S.Type -> Codegen Operand
    globalToOperand name t = do
      let llvmT= typeToLLVM t
      al <- alloca llvmT
      store al $ cons (C.GlobalReference llvmT (mkName (T.unpack name)))
      return al
    bls = createBlocks st
    st = execCodegen 
      (M.mapWithKey globalToOperand env) $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \a -> do
          var <- alloca (typeToLLVM (snd a))
          store var (local (AST.mkName (T.unpack (fst a))))
          assign (fst a) var
        b <- cgen body
        if retType /= S.TUnit
           then ret b
           else terminator . Do $ Ret Nothing []

makeName :: T.Text -> Name
makeName = AST.mkName . T.unpack

mkParameters args = ([Parameter ty nm [] | (ty, nm) <- args], False)

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
  defineInline VoidType "poke"
    [ (T.ptr (T.IntegerType 8),mkName "ptr")
    , (T.IntegerType 64, mkName "offset")
    , (T.IntegerType 8, mkName "val")] $
      createBlocks $ execCodegen M.empty $ do
        let ptr = LocalReference (T.ptr T.i8) (Name "ptr")
            offset = LocalReference T.i64 (Name "offset")
            val = LocalReference T.i8 (Name "val")
            cons0 = cons (C.Int 32 0)
        entry <- addBlock entryBlockName
        setBlock entry
        valPtr <- instr $ GetElementPtr True ptr [offset] []
        store valPtr val
        terminator $ Do $ Ret Nothing []

  let c = typeToLLVM (S.TType "Char" [])
  let n = typeToLLVM (S.TType "Num" [])
  external VoidType "putchar" [(c, mkName "in" )]
  external VoidType "puts" [(T.ptr c, mkName "in" )]
  external (T.ptr (T.IntegerType 8)) "malloc" [(n, Name "size" )]
  -- external (T.ptr (T.IntegerType 8)) "itoa" [(T.IntegerType 64, Name "size" )]
  -- define (typeToLLVM (S.TUnit)) "put2" [(c, mkName "in")] $
  --   createBlocks $ execCodegen M.empty $ do
  --     entry <- addBlock entryBlockName
  --     setBlock entry
  --     ret void

malloc = call (cons $ C.GlobalReference (T.ptr ((T.IntegerType 8))) (mkName "malloc")) [cons (C.Int 64 64)]

toSig :: [(Text, S.Type)] -> [(AST.Type, AST.Name)]
toSig = map (\(x,t) -> (typeToLLVM t, AST.mkName (T.unpack x)))

cgen :: S.CBlock -> Codegen AST.Operand
cgen S.CBlock {..} = last <$> mapM gen blockBody
  where
    isLocal :: Env -> Text -> Bool
    isLocal env name = case M.lookup name env of
                         Just _ -> True
                         _ -> False
    gen :: S.CExpr -> Codegen AST.Operand
    gen expr = case expr of
                 S.CCall fn args -> do
                   largs <- mapM cgen args
                   f <- getvar fn >>= load
                   call f largs
                       -- return f
                 (S.CAssign var st) -> do
                   ptr <- getvar var
                   res <- cgen st
                   store ptr res
                 (S.CLit pr) -> primCgen pr
                 S.CStruct name t fields -> do
                   -- small todo: figure out why there is a useless store instr
                   mal <- malloc
                   storage <- instr $ BitCast mal t []
                   parts <- mapM (cgen ) fields
                   forM (zip [0..] parts) $ \(i, v) -> do
                     indexPtr <- instr $ GetElementPtr True storage
                       [cons (C.Int 32 0), cons (C.Int 32 i)] []
                     store indexPtr v
                   return $ storage
                 S.CGet name i -> do
                   v <- getvar name >>= load
                   val <- instr $ GetElementPtr True v [cons (C.Int 32 0), cons (C.Int 32 i)] []
                   load val

                 (S.CVar name) -> getvar name >>= load
                 (S.CInit name t) -> do
                   var <- alloca (typeToLLVM t)
                   assign name var
                   return var
                 S.CScope bl -> do
                   modify $ \x -> x { symtab = MS.push (symtab x)}
                   ret <- cgen bl
                   modify $ \x -> x { symtab = MS.pop (symtab x)}
                   return ret
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
primCgen (S.Unit) = return $ cons $ C.Int 2 (toInteger 0)
primCgen (S.String str) = do

  nameInt <- gets globalName
  modify $ \x -> x { globalName = (globalName x + 1)}

  let arr = C.Array (T.IntegerType 8) (map constChar (T.unpack str))
  let arrT = T.ArrayType (fromInteger . toInteger . T.length $ str) (T.IntegerType 8)
  let name = "str" ++ show nameInt
      val = globalVariableDefaults 
        { name = mkName name
        , isConstant = False 
        , LLVM.AST.Global.type' = arrT
        , initializer = Just arr}

  modify $ \x -> x { toGlobal = M.insert (T.pack name) val (toGlobal x)}

  indexPtr <- instr $ GetElementPtr True (cons (C.GlobalReference arrT (mkName name)))
    [cons (C.Int 32 0), cons (C.Int 32 0)] []
  -- instr $ BitCast arr (T.ptr (T.IntegerType 8)) []
  return indexPtr

  where 
    constChar :: Char -> C.Constant
    constChar = C.Int 8 . toInteger . fromEnum
-- primCGen env (S.Tulple xs) = do
  -- parts <- mapM (cgen env) xs
  -- return $ C.Struct Nothing True parts

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: Env -> AST.Module -> [S.CFunc] -> IO AST.Module
codegen env mod fns = do -- withContext $ \context -> do
  pPrint newast
  return newast

  -- withModuleFromAST context newast $ \m -> do
    -- llstr <- moduleLLVMAssembly m
    -- return m
  where
    modn = mapM (codegenTop env) fns >> prelude
    newast = runLLVM mod modn

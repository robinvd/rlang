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
import Rlang.Scan (Env(..))
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
    , returnType = retType
    , basicBlocks = bls
    , visibility = if public then V.Default else V.Hidden}
  mapM_ (addDefn . GlobalDefinition . snd) (M.toList $ toGlobal st)
  where
    fnargs = toSig args
    globalToOperand :: Text -> S.Type -> Codegen Operand
    globalToOperand name t = do
      let llvmT= typeToLLVM env t
      al <- alloca llvmT
      store al $ cons (C.GlobalReference llvmT (mkName (T.unpack name)))
      return al
    bls = createBlocks st
    st = execCodegen 
      (M.mapWithKey globalToOperand (topFuncs env)) $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \(argName, argType) -> do
          var <- alloca (argType)
          store var (LocalReference argType (makeName argName))
          assign argName var
        b <- cgen body
        if retType /= T.VoidType
           then ret b
           else terminator . Do $ Ret Nothing []

makeName :: T.Text -> Name
makeName = AST.mkName . T.unpack

mkParameters args = ([Parameter ty nm [] | (ty, nm) <- args], False)

prelude :: LLVM ()
prelude = do
  let args = map (fmap (typeToLLVM mempty)) [("a", S.TType "Num" []),("b",S.TType "Num" [])]
  defineInline (T.ptr T.i64) "+" (toSig args) $
    createBlocks $ execCodegen M.empty $ do
      entry <- addBlock entryBlockName
      setBlock entry
      a <- load $ LocalReference (T.ptr T.i64) $ mkName "a"
      b <- load $ LocalReference (T.ptr T.i64) $ mkName "b"
      res <- (instr $ Add False False a b [])
      storage <- malloc 8
      castedPtr <- instr $ BitCast storage (T.ptr T.i64) []
      store castedPtr res
      ret castedPtr
  defineInline (T.ptr T.i64) "-" (toSig args) $
    createBlocks $ execCodegen M.empty $ do
      entry <- addBlock entryBlockName
      setBlock entry
      a <- load $ LocalReference (T.ptr T.i64) $ mkName "a"
      b <- load $ LocalReference (T.ptr T.i64) $ mkName "b"
      res <- (instr $ Sub False False a b [])
      storage <- malloc 8
      castedPtr <- instr $ BitCast storage (T.ptr T.i64) []
      store castedPtr res
      ret castedPtr
  defineInline VoidType "exitWithCode"
    [ (T.ptr T.i64, mkName "codePtr")] $
      createBlocks $ execCodegen M.empty $ do
        entry <- addBlock entryBlockName
        setBlock entry
        code <- load (LocalReference (T.ptr T.i64) $ mkName "codePtr")
        call (cons $ C.GlobalReference (T.FunctionType VoidType [T.i64] False) $ mkName "exit") [code]
        terminator $ Do $ Ret Nothing []
  defineInline T.i64 "toInt"
    [ (T.ptr T.i64, mkName "input")] $
      createBlocks $ execCodegen M.empty $ do
        entry <- addBlock entryBlockName
        setBlock entry
        code <- load (LocalReference (T.ptr T.i64) $ mkName "input")
        ret code
  defineInline T.i1 "cmpChar"
    [ (T.i8, mkName "a"), (T.i8, mkName "b")] $
      createBlocks $ execCodegen M.empty $ do
        entry <- addBlock entryBlockName
        setBlock entry
        r <- instrT T.i1 $ ICmp IP.EQ (LocalReference T.i8 $ mkName "a") (LocalReference T.i8 $ mkName "b") []
        terminator $ Do $ Ret (Just r) []

  defineInline VoidType "poke"
    [ (T.ptr (T.IntegerType 8),mkName "ptr")
    , (T.ptr T.i64 , mkName "offset")
    , (T.IntegerType 8, mkName "val")] $
      createBlocks $ execCodegen M.empty $ do
        let ptr = LocalReference (T.ptr T.i8) (Name "ptr")
            val = LocalReference T.i8 (Name "val")
            cons0 = cons (C.Int 32 0)
        entry <- addBlock entryBlockName
        setBlock entry
        offset <- load (LocalReference (T.ptr T.i64) $ mkName "offset")
        valPtr <- instrT (T.ptr T.i8) $ GetElementPtr True ptr [offset] []
        store valPtr val
        terminator $ Do $ Ret Nothing []

  let c = T.ptr T.i8
  external VoidType "putchar" [(T.i8, mkName "in" )]
  external VoidType "puts" [(c, mkName "in" )]
  external VoidType "exit" [(T.i64, mkName "code" )]
  external (T.ptr (T.IntegerType 8)) "malloc" [(T.i64, Name "size" )]
  external (T.ptr (T.i8)) "getline" []
  external ((T.i8)) "getchar" []
  -- external (T.ptr (T.IntegerType 8)) "itoa" [(T.IntegerType 64, Name "size" )]
  -- define (typeToLLVM (S.TUnit)) "put2" [(c, mkName "in")] $
  --   createBlocks $ execCodegen M.empty $ do
  --     entry <- addBlock entryBlockName
  --     setBlock entry
  --     ret void

malloc x = call (cons $ C.GlobalReference (T.ptr ((T.IntegerType 8))) (mkName "malloc")) [cons (C.Int 64 x)]

toSig :: [(Text, AST.Type)] -> [(AST.Type, AST.Name)]
toSig = map (\(x,t) -> (t, AST.mkName (T.unpack x)))

cgen :: S.CBlock -> Codegen AST.Operand
cgen S.CBlock {..} = last <$> mapM gen blockBody
  where
    gen :: S.CExpr -> Codegen AST.Operand
    gen expr = case expr of
      S.CCall fn args -> do
        largs <- mapM cgen args
        f <- getvar fn >>= load
        call f largs
      (S.CAssign var st) -> do
        ptr <- getvar var
        res <- cgen st
        store ptr res
      (S.CLit pr) -> primCgen pr
      S.CStruct name t fields -> do
        -- small todo: figure out why there is a useless store instr
        mal <- malloc 64
        storage <- instrT t $ BitCast mal t []
        parts <- mapM cgen fields
        forM (zip [0..] parts) $ \(i, v) -> do
          indexPtr <- instr $ GetElementPtr True storage
            [cons (C.Int 32 0), cons (C.Int 32 i)] []
          castedPtr <- instr $ BitCast indexPtr (T.ptr (T.ptr T.i64)) []
          store castedPtr v
          return ()
        return $ storage
      S.CGet name i -> do
        v <- getvar name >>= load
        val <- instr $ GetElementPtr True v [cons (C.Int 32 0), cons (C.Int 32 i)] []
        load val

      (S.CVar name) -> getvar name >>= load
      (S.CInit name t) -> do
        var <- alloca t
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
        -- test <- icmp IP.EQ (cons $ C.Int 64 0) cond
        cbr cond ifthen ifelse -- Branch based on the condition

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
        -- TODO no hardcode type
        phi T.i8 [(trval, ifthen), (flval, ifelse)]

      S.CWhile _ _ -> error "no while yet"
      -- (S.Ret x) -> do
      --   s <- stCgen x
      --   ret s
      --   return s

primCgen :: S.Prim -> Codegen AST.Operand
-- primCGen (String str) =
primCgen (S.Char ch) = return $ cons $ C.Int 8 (toInteger $ fromEnum ch)
primCgen (S.Num i) = do
  ptr <- malloc 8
  storage <- instr $ BitCast ptr (T.ptr T.i64) []
  store storage $ cons $ C.Int 64 (toInteger i)
  return storage
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
    modn = mapM (codegenTop env) fns >> prelude -- >> structs env
    newast = runLLVM mod modn

-- structs :: Env -> LLVM ()
-- structs (Env fns structs) = 

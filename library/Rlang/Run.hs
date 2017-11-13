{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Rlang.Run where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import System.IO
import System.Exit
import Text.Pretty.Simple (pPrint)
import qualified Data.Text.IO as T

import Rlang.Parsing
import Rlang.Scan
import Rlang.Syntax
import Rlang.Emit
import Rlang.Codegen
import Rlang.Core
import Rlang.Jit

-- | TODO
--   overall todo list
--   DONE
--   - [done] a setVar function
--   - [done] structs/data + types
--
--   FIXES
--   - letbinding/new variable keyword -> let works
--   - externs -> primitive version done
--   - [almost see next one] void type in llvm instead of int 1
--   - ability to return () and take () args
--   - type checking on constructors: StrLen("a", 1) works even if StrLn has 3 fields
--
--   feature list
--   - polymorphic data types
--   - Arrays/Vectors
--   - classes/polymophism
--   - clib
--   - ability to include llvm/c/asm code
--   - lambda's
--
--   notes:
--   - make polymorphic data structures have void* elements 
--     and cast them to the actual type when needed

isLeft (Left _) = True
isLeft _ = False

compile :: String -> IO (Env, [TopLevel])
compile file = do
  input <- T.readFile file
  let syntaxTree = parseTopLevel file input
  case syntaxTree of
    Left err -> print err >> exitFailure
    Right x -> do
      pPrint x
      let n = TType "Num" []
          scantotal :: (Env, [T.Text])
          scantotal = scanTop x
      modules <- mapM (compile . T.unpack) (snd scantotal)
      let scan = mconcat $ 

            [ (fst scantotal, x)
            , (Env (M.fromList 
              [("+", TFunc n [n, n])
              , ("-", TFunc n [n,n])
              , ("exitWithCode", TFunc TUnit [n])]
            ) mempty, mempty)
            ] ++ modules
          typeCheck = fmap (checkTop (fst scan)) x
          errs = filter isLeft $ concat typeCheck
      pPrint errs
      case any isLeft (concat typeCheck) of
        True -> putStrLn "type Error" >> exitFailure
        False -> do
          putStrLn "program type checks"
          return scan

run :: String -> IO ()
run file = do
  (scan, toplvl) <- compile file
  let core = toCore scan toplvl
  pPrint core
  a <- codegen scan (emptyModule "test") (core)
  runJIT a
  return ()

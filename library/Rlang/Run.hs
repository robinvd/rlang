{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Rlang.Run where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import System.IO
import Text.Pretty.Simple (pPrint)

import Rlang.Parsing
import Rlang.Scan
import Rlang.Syntax
import Rlang.Emit
import Rlang.Codegen
import Rlang.Core
import Rlang.Jit

-- | TODO
--   overall todo list
--   FIXES
--   - letbinding/new variable keyword
--   - actually start a new scope in Emit/cgen
--   - function pointers
--   - void type in llvm instead of int 1
--   - externs
--
--   feature list
--   - structs/data + types
--   - classes/polymophism
--   - clib
--   - ability to include llvm/c/asm code

isLeft (Left _) = True
isLeft _ = False

run :: T.Text -> IO ()
run input = do
  let syntaxTree = parseTopLevel input
  case syntaxTree of
    Left err -> print err
    Right x -> do
      pPrint x
      let n = TType "Num" []
          scan :: Env
          scan = scanTop x `M.union` M.fromList [("+", TFunc n [n, n])]
          typeCheck = fmap (checkTop scan) x
          -- errs = filter isLeft $ concat typeCheck
      pPrint $ typeCheck
      case any isLeft (concat typeCheck) of
        True -> putStrLn "type Error"
        False -> do
          putStrLn "program type checks"
          let core = toCore scan x
          pPrint core
          a <- codegen scan (emptyModule "test") core
          runJIT a
          return ()

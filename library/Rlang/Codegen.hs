{-# LANGUAGE OverloadedStrings #-}

module Rlang.Codegen where

import           Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha)
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Foldable (foldMap)

import Rlang.Core
import Rlang.Syntax

includes :: [Text]
includes = 
  [ "#include <stdio.h>"
  , "#define set(a, b) (a = b)"
  , "typedef char* String;"
  , "typedef char Char;"
  , "typedef int Int;"
  , "Int plus(Int a, Int b) {return(a + b);}"
  , "Int min(Int a, Int b) {return(a - b);}"
  , "Int eq(Int a, Int b) {return(a == b);}"
  , "Int mod(Int a, Int b) {return(a % b);}"
  , "Int not(Int a) {return(!a);}"]
  

codegenTop :: Core -> Text
codegenTop = T.unlines . (includes ++) . fmap codegen

codegenType :: Type -> Text
codegenType x = case x of

  TType a -> a
  TUnit -> "void"
  TVar a -> "void*"
  TArr a -> "TODO"--TODO
  TFunc retT args -> "void*"

genArgs :: [(Text, Type)] -> Text
genArgs args = T.concat . intersperse ", " . map (\(name, t) -> T.concat [codegenType t, " ", name] ) $ args

codegen :: CFunc -> Text
codegen (CFunc t origName name args body)  =
  T.concat $ intersperse " "
    [ codegenType t
    , name
    , "("
    , genArgs args
    , ") {"
    , T.concat . map codegenExpr $ body
    , "}"]

genPrim :: Prim -> Text
genPrim x = case x of

  String a -> T.pack $ show a
  Char a -> T.pack $ show a
  Num a -> T.pack $ show a
  -- Tulple?
  -- Unit = 

codegenExpr :: CExpr -> Text
codegenExpr x = case x of

  CStatement x -> codegenStatement x <> ";"

  CIf cond t f -> T.concat 
    ["if("
    , codegenStatement cond
    , "){"
    , T.concat . map codegenExpr $ t
    , "} else { "
    , T.concat . map codegenExpr $ f
    , "}"]

  CWhile pred body -> T.concat
    [ "while ("
    , codegenStatement pred
    , "){ "
    , T.concat . map codegenExpr $ body
    , "}"]

  CScope body ->
    "do {" <>
    foldMap codegenExpr body <>
    "} while (0);"

codegenStatement :: CStatement -> Text
codegenStatement s = case s of
  CCall name args -> if T.all (isAlpha) name
    then T.concat 
      [name
      , "("
      , T.concat $ intersperse " , " $ map codegenStatement args
      , ")"]
    else T.concat 
      ["infix" `T.append` T.concatMap (T.pack . show . fromEnum) name
      , "("
      , T.concat $ intersperse ", " $ map codegenStatement args
      , ")"]  -- [codegenExpr left, " ", name, " ", codegenExpr right]

  CAssign var st-> T.concat [var, " = ", codegenStatement st]

  CLit pr -> genPrim pr

  CVar name -> name

  CInit name t -> T.concat [codegenType t, " ", name]

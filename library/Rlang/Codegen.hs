{-# LANGUAGE OverloadedStrings #-}

module Rlang.Codegen where

import           Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha)
import Data.List (intersperse)

import Rlang.Syntax

includes :: [Text]
includes = 
  [ "#include <stdio.h>"
  , "typedef char* String;"
  , "typedef char Char;"
  , "typedef int Int;"
  , "Int plus(Int a, Int b) {return(a + b);}"
  , "Int min(Int a, Int b) {return(a - b);}"
  , "Int eq(Int a, Int b) {return(a == b);}"
  , "Int mod(Int a, Int b) {return(a % b);}"]
  

codegenTop :: [TopLevel] -> Text
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

codegen :: TopLevel -> Text
codegen x = case x of

  Function t name args body -> T.concat $ intersperse " "
    [ codegenType t
    , name
    , "("
    , genArgs args
    , ") { return("
    , codegenExpr body
    , "); }"]
  Binary t name args body -> codegen $ Function t ("infix" `T.append` T.concatMap (T.pack . show . fromEnum) name) args body
  _ -> "/* (TODO) */"

genPrim :: Prim -> Text
genPrim x = case x of

  String a -> T.pack $ show a
  Char a -> T.pack $ show a
  Num a -> T.pack $ show a
  -- Tulple?
  -- Unit = 

codegenExpr :: Expression -> Text
codegenExpr x = case x of

  FCall name args -> if T.all (isAlpha) name

    then T.concat [name, "(", T.concat $ intersperse ", " $ map codegenExpr args, ")"]
    else T.concat 
      ["infix" `T.append` T.concatMap (T.pack . show . fromEnum) name
      , "("
      , T.concat $ intersperse ", " $ map codegenExpr args
      , ")"]  -- [codegenExpr left, " ", name, " ", codegenExpr right]

  -- Lambda

  Var name -> name

  Lit pr -> genPrim pr

  -- Assignment var expr -> T.concat [var, " = ", codegenExpr expr]

  Let varName t value body -> T.concat $ intersperse " "
    [ codegenType t
    , varName
    , "="
    , codegenExpr value
    , ";"
    , codegenExpr body]

  If cond t f -> T.concat ["(", codegenExpr cond, " ? ", codegenExpr t, " : ", codegenExpr f, ")"]

  While pred body -> T.concat
    [ "while ("
    , codegenExpr pred
    , "){ "
    , codegenExpr body
    , "}"]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rlang.Core where

import Data.Maybe
import qualified Data.Map as M
import qualified LLVM.AST.Type as T
-- import Data.Char (isAlpha)
import Data.Text (Text)
-- import qualified Data.Text as T
-- import Data.Monoid ((<>))

import Rlang.Syntax
import Rlang.Scan

type Core = [CFunc]

data CFunc = CFunc
  { retType :: Type
  , origName :: Text
  , name :: Text
  , args :: [(Text, Type)]
  , body :: CBlock}
  deriving (Show)

data CBlock = CBlock
  { blockType :: Type
  , blockBody :: [CExpr]}
  deriving (Show)

data CExpr
  = CCall Text [CBlock]
  | CInit Text Type
  | CAssign Text CBlock
  | CLit Prim
  -- name llvmType items
  | CStruct Text T.Type [CBlock]
  | CVar Text
  | CWhile CBlock CBlock
  | CIf CBlock CBlock CBlock
  | CScope CBlock
-- | Ret CStatement
  deriving (Show)

typeToLLVM :: Type -> T.Type
typeToLLVM t = case t of
                 TType "Char" [] -> T.IntegerType 8
                 TType "Num" [] -> T.IntegerType 64
                 -- S.TUnit -> VoidType
                 TUnit -> T.IntegerType 1
                 TFunc ret args -> T.ptr $
                   T.FunctionType (typeToLLVM ret) (map typeToLLVM args) False
                 TType "Ptr" [x] -> T.ptr (typeToLLVM x)
                 TStruct _ xs -> T.StructureType False (map typeToLLVM xs)
                 _ -> error "Unkown type"

toCore :: Env -> [TopLevel] -> Core
toCore env = mapMaybe f
  where
  f :: TopLevel -> Maybe CFunc
  f (Function retType name args fBody) = Just CFunc {..}
    where
      origName = name
      body = syntaxToCore env (M.fromList args) fBody
  f _ = Nothing

syntaxToCore :: Env -> Env -> [Expression] -> CBlock
syntaxToCore env local body = CBlock (typeOf env local body) (map single body)
  where
    single x = case x of
            -- TODO args of FCall be [Expr]
            FCall name b -> CCall name (map (syntaxToCore env local) (map (:[]) b))
            Var name -> CVar name
            Lit pr -> CLit pr
            Struct name fields -> 
              CStruct 
                name 
                (typeToLLVM $ typeOf env local [Struct name fields])
                (map (syntaxToCore env local . (:[])) fields)
            Let varName varType varVal b -> CScope letBody
              where
                letBody = CBlock (typeOf env local b) $ 
                  [ CInit varName varType
                  , CAssign varName (syntaxToCore env local [varVal])]
                  ++ map single b
            If cond t f -> CIf 
                  (syntaxToCore env local cond)
                  (syntaxToCore env local t)
                  (syntaxToCore env local f)
            While cond b -> CWhile
                  (syntaxToCore env local cond)
                  (syntaxToCore env local b)


findType :: Env -> Env -> Text -> Type
findType env local name = case M.lookup name local of
                            Just x -> x
                            Nothing -> fromJust $ M.lookup name env 

typeOf :: Env -> Env -> [Expression] -> Type
typeOf env local body = case last body of
                          FCall name _ -> findType env local name
                          Var name -> findType env local name
                          Lit pr -> case runCheck env (checkPrim local pr) of
                                      Left x -> error $ show x
                                      Right x -> x
                          Struct name fields -> 
                            case runCheck env (mapM (check local) fields) of
                              Left x -> error $ show x
                              Right x -> TStruct name x
                          Let varName t _ b -> 
                            typeOf env (M.insert varName t local) b
                          If _ t _ -> typeOf env local t
                          While _ b -> typeOf env local b

                          

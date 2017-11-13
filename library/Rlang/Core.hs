{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rlang.Core where

import Control.Applicative
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
  { retType :: T.Type
  , public :: Bool
  , origName :: Text
  , name :: Text
  , args :: [(Text, T.Type)]
  , body :: CBlock}
  deriving (Show)

data CBlock = CBlock
  { blockType :: Type
  , blockBody :: [CExpr]}
  deriving (Show)

data CExpr
  = CCall Text [CBlock]
  | CInit Text T.Type
  | CAssign Text CBlock
  | CLit Prim
  -- name llvmType items
  | CStruct Text T.Type [CBlock]
  | CGet Text Integer
  | CVar Text
  | CWhile CBlock CBlock
  | CIf CBlock CBlock CBlock
  | CScope CBlock
-- | Ret CStatement
  deriving (Show)

typeToLLVM :: Env -> Type -> T.Type
typeToLLVM env t = case t of
                 TType "Char" [] -> T.IntegerType 8
                 TType "Num" [] -> T.ptr T.i64
                 TType "Int" [] -> T.i64
                 TType "Bool" [] -> T.i1
                 TUnit -> T.VoidType
                 -- TUnit -> T.IntegerType 1
                 TFunc ret args -> T.ptr $
                   T.FunctionType (typeToLLVM env ret) (map (typeToLLVM env) args) False
                 TType "Ptr" [x] -> T.ptr (typeToLLVM env x)

                 -- TODO make a llvm named struct
                 TType name _ -> 
                   let structXs = case M.lookup name (namedStructs env) of
                         Just x -> x
                         Nothing -> error "not found"
                    in T.ptr (T.StructureType False (map (typeToLLVM env) structXs))
                 TVar _ -> T.ptr (T.i8)
                 x -> error $ "Unknown type: " ++ show x

toCore :: Env -> [TopLevel] -> Core
toCore env = mapMaybe f
  where
  f :: TopLevel -> Maybe CFunc
  f (Function attr retTypeCore name argsCore fBody) = Just CFunc {..}
    where
      args = fmap (fmap (typeToLLVM env)) argsCore
      retType = typeToLLVM env retTypeCore
      public = "export" `elem` attr
      origName = name
      body = syntaxToCore env (M.fromList argsCore) fBody
  f _ = Nothing

syntaxToCore :: Env -> M.Map Text Type -> [Expression] -> CBlock
syntaxToCore env local body = CBlock (typeOf env local body) (map (single env local) body)
  where
    single env local x = case x of
            -- TODO args of FCall be [Expr]
            FCall name b -> CCall name (map (syntaxToCore env local) (map (:[]) b))
            Var name -> CVar name
            Lit pr -> CLit pr
            Struct name fields -> 
              CStruct 
                name 
                (typeToLLVM env $ typeOf env local [Struct name fields])
                (map (syntaxToCore env local . (:[])) fields)
            GetNum name i -> CGet name i
            Let varName varType varVal b -> CScope letBody
              where
                newLocal = M.insert varName varType local
                letBody = CBlock (typeOf env newLocal b) $ 
                  [ CInit varName (typeToLLVM env varType)
                  , CAssign varName (syntaxToCore env local [varVal])]
                  ++ map (single env newLocal) b
            Assign varName val -> CAssign varName (syntaxToCore env local [val])
              where t = findType env local varName
            If cond t f -> CIf 
                  (syntaxToCore env local cond)
                  (syntaxToCore env local t)
                  (syntaxToCore env local f)
            While cond b -> CWhile
                  (syntaxToCore env local cond)
                  (syntaxToCore env local b)


findType :: Env -> M.Map Text Type -> Text -> Type
findType env local name = case M.lookup name local <|> M.lookup name (topFuncs env) of
                            Just x -> x
                            Nothing -> error $ "tried to find: " ++ show name ++ " \n\n in: \nlocal: " ++ show local ++ "\nglobal:" ++ show env

typeOf :: Env -> M.Map Text Type -> [Expression] -> Type
typeOf env local body = case last body of

  FCall name _ -> findType env local name
  Var name -> findType env local name
  Lit pr -> case runCheck env (checkPrim local pr) of
              Left x -> error $ show x
              Right x -> x
  Struct name fields -> 
    case runCheck env (mapM (check local) fields) of
      Left x -> error $ show x
      Right x -> TType name x
  GetNum name i -> case findType env local name of

    TType structName _ -> case M.lookup structName (namedStructs env) of
                            Just x -> x !! fromInteger i
      -- if length args <= fromInteger i
      -- then error "empty index in typeof"
      -- else args !! (fromInteger i)
    _ -> error "not struct"
  Let varName t _ b -> 
    typeOf env (M.insert varName t local) b
  If _ t _ -> typeOf env local t
  While _ b -> typeOf env local b


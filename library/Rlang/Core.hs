{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rlang.Core where

import Data.Maybe
import qualified Data.Map as M
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
  = CWhile CBlock CBlock
  | CIf CBlock CBlock CBlock
  | CScope CBlock
  | CCall Text [CBlock]
  | CAssign Text CBlock
  | CLit Prim
  | CVar Text
  | CInit Text Type
-- | Ret CStatement
  deriving (Show)

-- toReturn :: (CStatement -> CExpr) -> Expression -> [CExpr]
-- toReturn final input = case input of
--   FCall name args ->
--     let a = zip3 [0..] args (repeat $ TType "Int") in
--       return $ CScope $ concatMap parseArgs a <>
--         [final (CCall name (map (CVar . parseCall) (map fst3 a)))]
--   Var x -> [final $ CVar x]
--   Lit x -> [final $ CLit x]
--   -- TODO make new scope
--   Let var t val body ->
--     [ CStatement $ CInit var t]
--     ++ toReturn (CStatement . CAssign var) val
--     ++ toReturn final body
--   If cond t f -> return . CScope $
--     [CStatement (CInit "_cond" (TType "Int"))] ++
--     toReturn (CStatement . CAssign "_cond") cond ++
--     [CIf (CVar "_cond") (toReturn final t) (toReturn final f)]
--   While cond body -> return . CScope $
--     [CStatement (CInit "_cond" (TType "Int"))] ++
--     toReturn (CStatement . CAssign "_cond") cond ++
--     [CWhile (CVar "_cond") (toReturn final body)]
--   where
--     fst3 (a,_,_) = a
--     parseArgs :: (Int, Expression, Type) -> [CExpr]
--     parseArgs (num, arg, t) =
--       let name = "_arg" <> (T.pack . show $ num) in
--         CStatement (CInit name t) :
--         toReturn (CStatement . CAssign name)
--             arg
--     parseCall :: Int -> Text
--     parseCall x = "_arg" <> (T.pack . show $ x)

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
            Let varName varType varVal b -> CScope body
              where
                body = CBlock (typeOf env local b) $ 
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
                          Lit pr -> checkPrim pr
                          Let varName t _ b -> 
                            typeOf env (M.insert varName t local) b
                          If _ t _ -> typeOf env local t
                          While _ b -> typeOf env local b

                          

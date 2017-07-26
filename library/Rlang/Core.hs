{-# LANGUAGE OverloadedStrings #-}

module Rlang.Core where

import Data.Text (Text)
import Data.Maybe
import Data.Char (isAlpha)
import qualified Data.Text as T
import Data.Monoid ((<>))

import Rlang.Syntax

type Core = [CFunc]

data CFunc = CFunc
  { retType :: Type
  , origName :: Text
  , name :: Text
  , args :: [(Text, Type)]
  , body :: [CExpr]}
  deriving (Show)

data CExpr
  = CStatement CStatement
  | CWhile CStatement [CExpr]
  | CIf CStatement [CExpr] [CExpr]
  | CScope [CExpr]
  deriving (Show)

data CStatement
  = CCall Text [CStatement]
  | CAssign Text CStatement
  | CLit Prim
  | CVar Text
  | CInit Text Type
  | Ret CStatement
  deriving (Show)

toReturn :: (CStatement -> CExpr) -> Expression -> [CExpr]
toReturn final input = case input of
  FCall name args ->
    let a = zip3 [0..] args (repeat $ TType "Int") in
      return $ CScope $ concatMap parseArgs a <>
        [final (CCall name (map (CVar . parseCall) (map fst3 a)))]
  Var x -> [final $ CVar x]
  Lit x -> [final $ CLit x]
  -- TODO make new scope
  Let var t val body ->
    [ CStatement $ CInit var t]
    ++ toReturn (CStatement . CAssign var) val
    ++ toReturn final body
  If cond t f -> return . CScope $
    [CStatement (CInit "_cond" (TType "Int"))] ++
    toReturn (CStatement . CAssign "_cond") cond ++
    [CIf (CVar "_cond") (toReturn final t) (toReturn final f)]
  While cond body -> return . CScope $
    [CStatement (CInit "_cond" (TType "Int"))] ++
    toReturn (CStatement . CAssign "_cond") cond ++
    [CWhile (CVar "_cond") (toReturn final body)]
  where
    fst3 (a,_,_) = a
    parseArgs :: (Int, Expression, Type) -> [CExpr]
    parseArgs (num, arg, t) =
      let name = "_arg" <> (T.pack . show $ num) in
        CStatement (CInit name t) :
        toReturn (CStatement . CAssign name)
            arg
    parseCall :: Int -> Text
    parseCall x = "_arg" <> (T.pack . show $ x)

toCore :: [TopLevel] -> Core
toCore = mapMaybe f
  where
  f :: TopLevel -> Maybe CFunc
  f (Function retType name args body) =
    Just $ CFunc retType name name args $ toReturn
    (\x -> CStatement $ Ret x) body
    where
      -- name' = if not (T.all isAlpha name)
      --   then "infix" <> T.concatMap (T.pack . show . fromEnum) name
      --   else name
  f (Binary retType name args body) =
    f (Function retType name args body)
  f _ = Nothing

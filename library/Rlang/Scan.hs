{-# LANGUAGE OverloadedStrings #-}
module Rlang.Scan where

import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Monoid (mconcat, Monoid)
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad

import Rlang.Syntax

type Env = M.Map Text Type

scanTop :: [TopLevel] -> Env
scanTop = mconcat . fmap scan

scan :: TopLevel -> Env
scan x = case x of
  Function ret name args _ -> M.singleton name (TFunc ret (map snd args))
  Binary ret name args expr -> scan $ Function ret name args expr
  Extern _ ret name args -> M.singleton name (TFunc ret args)
  Import package -> M.empty -- TODO

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Text
  | InteralError Text
  deriving (Show)

type Check = ExceptT TypeError (Reader Env)

checkPrim :: Prim -> Type
checkPrim p = case p of

  String _ -> TType "String"
  Char _ -> TType "Char"
  Num _ -> TType "Num"
  Tulple xs -> TTulple . fmap checkPrim $ xs
  Unit -> TUnit

-- lookupCheck :: Text -> Check Type
-- lookupCheck x = do
--   env <- ask
--   case M.lookup x env of
--     Just a -> return a
--     Nothing -> throwError $ NotInScope x

lookupF :: Map Text Type -> Text -> Check Type
lookupF local x = case M.lookup x local of

  Just a -> return a
  Nothing -> do
    env <- ask
    case M.lookup x env of
      Just a -> return a
      Nothing -> throwError $ NotInScope $ T.append x $ T.pack $ show local

checkT :: TopLevel -> Check Type
checkT x = case x of

  Function ret name args body -> check (M.fromList args) body
  _ -> throwError $ InteralError "Not a function"

fits :: [Type] -> [Type] -> Maybe (Map Text Type)
fits fargs input = f M.empty fargs input
  where
    -- binded TVar -> fargs -> inputargs
    f :: Map Text Type -> [Type] -> [Type] -> Maybe (Map Text Type)
    f env [] [] = Just env
    f env (TVar x:xs) (y:ys) = case M.lookup x env of

      Just a -> if a == y
        then f env xs ys
        else Nothing
      Nothing -> f (M.insert x y env) xs ys
    f env (x:xs) (y:ys) = case x == y of
      True -> f env xs ys
      False -> Nothing
    f _ _ _ = Nothing

check :: Map Text Type -> Expression -> Check Type
check local expr =
  case expr of

    FCall name args -> do
      TFunc ret ar <- lookupF local name -- Type TODO partial let binding
      a <- mapM (check local) args -- [Type]
      let matched = fits ar a

      case matched of
        Nothing -> throwError $ Mismatch (TArr a) (TArr ar)
        Just x -> case ret of

          TVar v -> case M.lookup v x of
            Just res -> return res
            Nothing -> throwError $ InteralError "Map lookup failed"
          _ -> return ret

    -- BinOp name arg1 arg2 -> check local (FCall name [arg1, arg2])

    -- Lambda -- TODO

    Var name -> lookupF local name

    Lit prim -> return $ checkPrim prim

    -- Assignment _ _ -> return TUnit

    If p t f -> do
      p' <- check local p
      t' <- check local t
      f' <- check local f
      unless (p' /= TType "Bool") $ throwError $ Mismatch (TType "Bool") p'
      unless (t' == f') $ throwError $ Mismatch t' f'
      return t'

runCheck :: Env -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: Env -> TopLevel -> Either TypeError Type
checkTop env x = runCheck env $ checkT x

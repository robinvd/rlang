{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Rlang.Scan where

import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Monoid (mconcat, Monoid)
import           Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad
import Control.Applicative

import Rlang.Syntax

data Env = Env
  { topFuncs :: M.Map Text Type
  , namedStructs :: M.Map Text [Type]} deriving (Show)

instance Monoid Env where
  mempty = Env mempty mempty
  mappend (Env a b) (Env x y) = Env (mappend a x) (mappend b y)

scanTop :: [TopLevel] -> (Env, [Text])
scanTop = mconcat . fmap scan

scan :: TopLevel -> (Env, [Text])
scan x = case x of

  Function attr ret name args _ -> 
    (Env (M.singleton name (TFunc ret (map snd args))) mempty, mempty)
  -- Binary ret name args expr -> scan $ Function ret name args expr
  Extern _ ret name args -> (Env (M.singleton name (TFunc ret args)) mempty, mempty)
  Import package -> (mempty, [package])
  StructDeclare name typeArgs constructor fieldTypes -> 
    (Env 
      (M.singleton constructor (TFunc (TType name (map TVar typeArgs)) fieldTypes)) 
      (M.singleton name (fieldTypes))
    , mempty)

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Text
  | InteralError Text
  deriving (Show)

type Check = ExceptT TypeError (Reader Env)

checkPrim :: M.Map Text Type -> Prim -> Check Type
checkPrim env p = case p of

  String _ -> return $ TType "Ptr" [TType "Char" []]
  Char _ -> return $ TType "Char" []
  Num _ -> return $ TType "Num" []
  -- Tulple xs -> do
    -- types <- mapM (check env ) xs
    -- return $ TTulple types
  Unit -> return $ TUnit

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
    env <- asks topFuncs
    case M.lookup x env of
      Just a -> return a
      Nothing -> throwError $ NotInScope $ T.append x $ T.pack $ show local

checkT :: TopLevel -> [Check Type]
checkT x = case x of

  Function attr ret name args body -> map (check (M.fromList args)) body ++ [funcSigCheck]
    where
      funcSigCheck = f =<< check (M.fromList args) (last body)
      f :: Type -> Check Type
      f x
        | x == ret = return x
        | otherwise = throwError $ Mismatch x ret
  Extern pkgs ret name args -> return $ return ret
        
  _ -> []-- return $ throwError $ InteralError "Not a function"

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

    Lit prim -> checkPrim local prim

    Struct name xs -> 
      return . TType name =<< mapM (check local) xs

    -- Get
    GetNum name field -> do
      structTab <- asks namedStructs
      l <- lookupF local name 
      case l of
        TType name xs -> case M.lookup name structTab of
                           Just xs -> return $ xs !! fromInteger field
                           Nothing -> error $ "struct \"" ++ show name ++ "\" not found"
          -- if length xs <= fromInteger field
          --                   then error $ "index too big at check " ++ show l 
          --                   else return $ xs !! (fromInteger field)
        _ -> error $ show l ++ " in " ++ show local -- throwError $ undefined

    Let varName t val rest -> check (M.insert varName t local) (last rest)
    -- Assignment _ _ -> return TUnit

    If p t f -> do
      -- TODO not just the last one
      p' <- check local (last p)
      t' <- check local (last t)
      f' <- check local (last f)
      when (p' /= TType "Bool" []) $ throwError $ Mismatch (TType "Bool" []) p'
      when (t' /= f') $ throwError $ Mismatch t' f'
      return t'

runCheck :: Env -> Check a -> Either TypeError a
runCheck env = flip runReader env . runExceptT

checkTop :: Env -> TopLevel -> [Either TypeError Type]
checkTop env x = map (runCheck env) (checkT x)

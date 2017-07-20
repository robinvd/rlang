{-# LANGUAGE OverloadedStrings #-}

module Rlang.Core where

import Data.Text (Text)

import Rlang.Syntax

type Core = [CFunc]

data CFunc = CFunc 

  { origName :: Text
  , name :: Text
  , args :: [(Text, Type)]
  , body :: CExpr}

data CExpr
  = CCall Text CExpr
  | CAssign Text CExpr
  | CWhile 
  | CIf

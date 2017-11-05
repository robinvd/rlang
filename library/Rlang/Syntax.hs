module Rlang.Syntax where

import Data.Text (Text)

data Prim
  = String Text
  | Char Char
  | Num Int
  -- | Tulple [Expression]
  | Unit
  deriving (Show)

data Type
  = TType Text [Type]
  | TUnit
  -- variable in a type ex. forall a. a -> a
  | TVar Text
  | TStruct Text [Type]
  | TArr [Type]
  | TFunc Type [Type]
  deriving (Show, Eq)


  -- TForall Text

data Expression
  = FCall Text [Expression]
  -- | BinOp Text Expression Expression
  -- | Lambda Type Text Expression
  | Var Text
  | Lit Prim
  | Struct Text [Expression]

  -- get a field from a struct
  -- name fieldToGet
  | Get Text Text

  -- name retType assignment letBody
  | Let Text Type Expression [Expression]
  | Assign Text Expression
  | If [Expression] [Expression] [Expression]
  | While [Expression] [Expression]
  deriving (Show)

data TopLevel

  -- define a new function
  -- retType Name arguments body
  = Function Type Text [(Text, Type)] [Expression]

  -- define a new binary function
  -- retType name arguments body
  -- | Binary Type Text [(Text, Type)] Expression

  -- package retType fName argTypes
  | Extern Text Type Text [Type]

  -- package
  | Import Text
  deriving (Show)


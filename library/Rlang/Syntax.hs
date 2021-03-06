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
  -- | TStruct Text [Type]
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
  | GetNum Text Integer

  -- name retType assignment letBody
  | Let Text Type Expression [Expression]
  | Assign Text Expression
  | If [Expression] [Expression] [Expression]
  | While [Expression] [Expression]
  deriving (Show)

data TopLevel

  -- define a new function
  -- attr retType Name arguments body
  = Function [Text] Type Text [(Text, Type)] [Expression]

  -- data declaration
  -- typeName typeVar ConstName TypesInStruct
  -- data Pair a b = MakePair a b
  -- becomes "Pair" ["a","b"] "MakePair" [TypeVar a, TypeVar b]
  | StructDeclare Text [Text] Text [Type]

  -- define a new binary function
  -- retType name arguments body
  -- | Binary Type Text [(Text, Type)] Expression

  -- package retType fName argTypes
  | Extern Text Type Text [Type]

  -- package
  | Import Text
  deriving (Show)


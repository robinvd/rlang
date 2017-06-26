{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE OverloadedStrings #-}

module Rlang.Parsing where

import           Text.Parsec (many, Parsec, ParsecT, choice, parse, eof, try)
import           Text.Parsec.Text ()
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import           Text.Parsec.Char
import           Data.Text (Text)
import qualified Data.Text as T

import Rlang.Lexer

data Prim
  = String Text
  | Char Char
  | Num Int
  deriving (Show)

data Type 
  = TNum
  | TString
  | TChar
  | TArr Type Type
  | TArrow Type Type
  | TForall [Text] Type
  deriving (Show)

data Val = Var Text | Lit Prim deriving (Show)

data Function = Func
  { retType :: Type
  , name :: Text
  , args :: [(Text, Type)]
  , body :: [Expression]
  } deriving (Show)

data Expression 
  = FCall Text [Expression]
  | Lambda Type Text Expression
  | Val Val 
  | Assignment Text Expression
  deriving (Show)

type Parser = Parsec Text () -- forall s u m. ParsecT s u m

int :: Parser Expression
int = Val . Lit . Num . fromInteger <$> integer

str :: Parser Expression
str = Val . Lit . String . T.pack <$> quotedString

-- binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc
-- 
-- table = [[binary "*" Times Ex.AssocLeft,
--           binary "/" Divide Ex.AssocLeft]
--         ,[binary "+" Plus Ex.AssocLeft,
--           binary "-" Minus Ex.AssocLeft]]
table = [[]]

expr :: Parser Expression
expr = Ex.buildExpressionParser table factor

variable :: Parser Expression
variable = Val . Var . T.pack <$> identifier

parseType :: Parser Type
parseType = choice
  [ Tok.symbol lexer "Num" >> return TNum
  , Tok.symbol lexer "String" >> return TString
  , Tok.symbol lexer "Char" >> return TChar]

varType :: Parser (Text, Type)
varType = do
  i <- T.pack <$> identifier
  Tok.colon lexer
  t <- parseType
  return (i, t)

function :: Parser Function
function = do
  t <- parseType
  name <- T.pack <$> identifier
  args <- parens $ commaSep $ varType
  body <- braces . many $ do 
    x <- expr
    reservedOp ";"
    return x
  return $ Func t name args body

call :: Parser Expression
call = do
  name <- T.pack <$> identifier
  args <- parens . commaSep $ expr
  return $ FCall name args

assign :: Parser Expression
assign = do
  v <- T.pack <$> identifier
  Tok.symbol lexer "="
  val <- expr
  return $ Assignment v val

factor :: Parser Expression
factor = choice $ fmap try
    [ call
    , assign
    , int
    , str
    -- , function
    , variable
    , parens expr]

contents :: Parsec Text () a -> Parsec Text () a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parsec Text () [Function]
toplevel = do
  def <- many function
  -- reservedOp ";"
  return def

parseTopLevel = parse (contents toplevel) "<stdin>"

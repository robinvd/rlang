{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Rlang.Parsing where

import           Text.Parsec ((<|>), many, Parsec, ParsecT, choice, parse, eof, try, optionMaybe)
import           Text.Parsec.Text ()
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import           Data.Text (Text)
import qualified Data.Text as T

import Rlang.Syntax
import Rlang.Lexer


type Parser = Parsec Text () -- forall s u m. ParsecT s u m

int :: Parser Expression
int = Lit . Num . fromInteger <$> integer

str :: Parser Expression
str = Lit . String <$>  quotedString

binop = Ex.Infix ((\x y z -> FCall x [y,z]) <$> operator) Ex.AssocRight

-- binary :: Text -> Ex.Assoc -> Ex.Operator Text () Data.Funcor.Identity (Expression -> Expression -> Expression)
binary s assoc = Ex.Infix (reservedOp s >> return ((\x y z -> FCall x [y,z]) (T.pack s))) assoc

-- table = [[binary "*" Times Ex.AssocLeft,
--           binary "/" Divide Ex.AssocLeft]
--         ,[binary "+" Plus Ex.AssocLeft,
--           binary "-" Minus Ex.AssocLeft]]
binops = [[binary "=" Ex.AssocLeft]] -- [[binary ";" Semi Ex.AssocLeft]]

expr :: Parser Expression
expr = Ex.buildExpressionParser (binops ++ [[binop]]) factor

variable :: Parser Expression
variable = Var <$> identifier

parseType :: Parser Type
parseType = choice $ fmap try
  [ symbol "(" >> symbol ")" >> return TUnit
  , do 
    types <- parens . commaSep $ parseType
    return $ TTulple types
  , TVar <$> lowIdentifier
  , TType <$> capIdentifier]

varType :: Parser (Text, Type)
varType = do
  i <- identifier
  Tok.colon lexer
  t <- parseType
  return (i, t)

function :: Parser TopLevel
function = do
  name <- identifier
  args <- parens . commaSep $ varType
  symbol ":"
  t <- parseType
  symbol "="
  body <- expr
  return $ Function t name args body

binDef :: Parser TopLevel
binDef = do
  reserved "binary"
  prio <- integer
  name <- operator
  args <- parens . commaSep $ varType
  symbol ":"
  t <- parseType
  symbol "="
  body <- expr
  return $ Binary t name args body

-- semiExpr :: Parser Expression
-- semiExpr = do
--   x <- expr
--   reservedOp ";"
--   return x

call :: Parser Expression
call = do
  name <- identifier
  args <- parens . commaSep $ expr
  return $ FCall name args

letbinding :: Parser Expression
letbinding = try $ do
  reserved "let"
  (name, t) <- varType
  reservedOp "="
  val <- expr
  body <- expr
  return $ Let name t val body

while :: Parser Expression
while = do
  reserved "while"
  cond <- expr
  body <- expr
  reserved "end"
  return $ While cond body

factor :: Parser Expression
factor = choice $ fmap try
    [ call
    , int
    , str
    , variable
    , parens expr
    -- , for
    ] ++ [parseIf, while, letbinding]

contents :: Parsec Text () a -> Parsec Text () a
contents p = do
  spaces
  r <- p
  eof
  return r

extern :: Parser TopLevel
extern = do
  reserved "extern"
  package <- quotedString
  name <- identifier
  args <- parens . commaSep $ parseType
  symbol ":"
  retType <- parseType
  return $ Extern package retType name args

imp :: Parser TopLevel
imp = do
  reserved "import"
  package <- identifier
  return $ Import package

parseIf :: Parser Expression
parseIf = do
  reserved "if"
  cond <- expr
  reserved "then"
  body <- expr
  reserved "else"
  elsebody <- expr
  reserved "end"
  return $ If cond body elsebody
  

toplevel :: Parsec Text () [TopLevel]
toplevel = many $ function <|> binDef <|> extern <|> imp

parseTopLevel = parse (contents toplevel) "<stdin>"

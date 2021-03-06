{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Rlang.Parsing where

import           Control.Monad
import           Text.Parsec ((<|>), unexpected, many, Parsec, choice, parse, eof, try, ParseError)
import           Text.Parsec.Text ()
import           Text.Parsec.Combinator
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
str = Lit . String <$> quotedString


char :: Parser Expression
char = Lit . Char <$> charLit

binop = Ex.Infix ((\x y z -> FCall x [y,z]) <$> operator) Ex.AssocRight

-- binary :: Text -> Ex.Assoc -> Ex.Operator Text () Data.Funcor.Identity (Expression -> Expression -> Expression)
binary s assoc = Ex.Infix (reservedOp s >> return ((\x y z -> FCall x [y,z]) (T.pack s))) assoc

-- table = [[binary "*" Times Ex.AssocLeft,
--           binary "/" Divide Ex.AssocLeft]
--         ,[binary "+" Plus Ex.AssocLeft,
--           binary "-" Minus Ex.AssocLeft]]
binops = [[binary "=" Ex.AssocLeft]] -- [[binary ";" Semi Ex.AssocLeft]]

exprSingle :: Parser Expression
exprSingle  = Ex.buildExpressionParser (binops ++ [[binop]]) factor

expr :: Parser [Expression]
expr = sepBy1 exprSingle (symbol ";")

variable :: Parser Expression
variable = Var <$> identifier

parseType :: Parser Type
parseType = choice $ fmap try
  [ symbol "()"  >> return TUnit
  , try $ do
    args <- parens . commaSep $ parseType
    symbol "->"
    t <- parseType
    return $ TFunc t args
  , do
    main <- capIdentifier
    rest <- many parseType
    return $ TType main rest
  , parens parseType
  , do
    types <- parens . commaSep $ parseType
    when (length types < 2) (unexpected "need atleast 2 for tulple")
    return $ TType "Tulple" types
  , TVar <$> lowIdentifier
  ]

varType :: Parser (Text, Type)
varType = do
  i <- identifier
  Tok.colon lexer
  t <- parseType
  return (i, t)

function :: Parser TopLevel
function = do
  attr <- many $ try $ symbol "export"
  name <- identifier
  args <- parens . commaSep $ varType
  symbol "->"
  t <- parseType
  symbol "="
  body <- expr
  return $ Function attr t name args body

binDef :: Parser TopLevel
binDef = do
  attr <- many $ try $ symbol "export"
  reserved "binary"
  prio <- integer
  name <- operator
  args <- parens . commaSep $ varType
  symbol "->"
  t <- parseType
  symbol "="
  body <- expr
  return $ Function attr t name args body

call :: Parser Expression
call = do
  name <- identifier
  args <- parens . commaSep $ exprSingle
  return $ FCall name args

letbinding :: Parser Expression
letbinding = try $ do
  reserved "let"
  (name, t) <- varType
  reservedOp "="
  val <- exprSingle
  body <- expr
  return $ Let name t val body

while :: Parser Expression
while = do
  reserved "while"
  cond <- expr
  body <- expr
  reserved "end"
  return $ While cond body

-- inline :: Parser TopLevel
-- inline = do
--   reserved "inline"
--   Inline <$> quotedString

assignment :: Parser Expression
assignment = do
  name <- identifier 
  symbol "="
  body <- exprSingle
  return $ Assign name body

structGet :: Parser Expression
structGet = do
  name <- identifier
  symbol "->"
  choice 
    [ do
      field <- identifier
      return $ Get name field
    , do
      num <- fromInteger <$> integer
      return $ GetNum name num
    ]

structInit :: Parser Expression
structInit = do
  strName <- capIdentifier
  args <- parens . commaSep $ exprSingle
  return $ Struct strName args



factor :: Parser Expression
factor = choice $ fmap try
    [ structInit
    , call
    , assignment
    , structGet
    , int
    , str
    , char
    , variable
    , parens factor
    , symbol "()" >> return (Lit Unit)
    , do
      x <- parens . commaSep $ factor
      return . Struct "Tulple" $ x
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
  symbol "->"
  retType <- parseType
  symbol ";"
  return $ Extern package retType name args

imp :: Parser TopLevel
imp = do
  reserved "import"
  package <- quotedString
  return $ Import package

struct :: Parser TopLevel
struct = do
  reserved "data"
  name <- capIdentifier
  para <- many $ try lowIdentifier
  symbol "="
  constr <- capIdentifier
  types <- parens $ commaSep $ parseType
  return $ StructDeclare name para constr types

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
toplevel = many $ choice $ fmap try
  [ struct
  , function 
  , binDef 
  , extern
  , imp] -- <|> inline

parseTopLevel :: String -> Text -> Either ParseError [TopLevel]
parseTopLevel = parse (contents toplevel)

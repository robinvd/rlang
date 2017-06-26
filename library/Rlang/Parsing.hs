{- LANGUAGE OverloadedStrings #-}

module Rlang.Parsing where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Text ()
import Data.Text (Text)
import qualified Data.Text as T

data RVal =
    Expression Expression
  | Value Val
  | Function Function deriving (Show)

data Type =
    Number
  | String
  | Char deriving (Show)

data Val = Var Text | Lit Text deriving (Show)

data Function = Func
  { retType :: Type
  , name :: Text
  , args :: [(Text, Type)]
  , body :: [Expression]
  } deriving (Show)

data Expression =
    Assignment Text Expression
  | FCall Text [Expression]
  | Val Val deriving (Show)

parseRVal = choice 
  [ -- Expression <$> parseExpression
    Function <$> parseFunction
  , Value <$> parseVal
  ]

parseVal = choice
  [ Var <$> parseLit
  , Lit <$> parseString
  , Lit . T.pack <$> many1 digit]

parseString = do
  char '"'
  x <- many $ noneOf "\""
  char '"'
  return $ T.pack x

parseLit :: Parsec Text () Text
parseLit = do
  x <- letter
  xs <- many alphaNum
  return $ T.pack $ x:xs

parseFunction = do
  t <- parseType
  spaces
  n <- parseLit
  spaces
  args <- parseArgs
  spaces
  b <- parseBody
  return $ Func t n args b
  where
    parseBody :: Parsec Text () [Expression]
    parseBody = between (char '{') (char '}') $ do
      spaces
      body <- endBy parseExpression (spaces >> char ';' >> spaces)
      spaces
      return body
    parseArgs = do
      char '('
      x <- flip sepBy (spaces >> char ',' >> spaces) $ do
        n <- parseLit
        spaces
        char ':'
        spaces
        t <- parseType
        return (n, t)
      char ')'
      return x

parseType :: Parsec Text () Type
parseType = choice
  [ string "Num" >> return Number
  , string "String" >> return String
  , string "Char" >> return Char]

parseExpression :: Parsec Text () Expression
parseExpression = choice [try parseFCall, try parseAssignment, try $ Val <$> parseVal]
  where
    parseAssignment = do
      n <- parseLit
      spaces
      char '='
      spaces
      v <- parseExpression
      return $ Assignment n v
    parseFCall = do
      n <- parseLit
      spaces
      args <- parseArgs
      return $ FCall n args
    parseArgs = between (char '(') (char ')') $
      sepBy parseExpression (spaces >> char ',' >> spaces) 

parse :: Text -> Either ParseError [Function]
parse = runParser (many $ spaces >> f) () "rlang"
  where 
  f :: Parsec Text () Function
  f = do
    x <- parseFunction
    spaces
    return x

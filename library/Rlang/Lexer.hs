{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}


module Rlang.Lexer where

import Text.Parsec (ParsecT, Stream, (<|>), many)
import Text.Parsec.Char
import Text.Parsec.Text ()
import qualified Data.Text as T (pack)
import           Data.Text (Text)

import qualified Text.Parsec.Token as Tok

myDef :: Stream s m Char => Tok.GenLanguageDef s u m
myDef = Tok.LanguageDef
               { Tok.commentStart   = ""
               , Tok.commentEnd     = ""
               , Tok.commentLine    = ""
               , Tok.nestedComments = True
               , Tok.identStart     = letter <|> char '_'
               , Tok.identLetter    = alphaNum <|> oneOf "_'."
               , Tok.opStart        = Tok.opLetter myDef
               , Tok.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , Tok.reservedOpNames= []
               , Tok.reservedNames  = ["let"]
               , Tok.caseSensitive  = True
               }

lexer :: Stream s m Char => Tok.GenTokenParser s u m
lexer = Tok.makeTokenParser style
  where
    ops = ["="]
    names = ["binary","import","extern","if","else","end","while","let"]
    style = myDef {
               Tok.commentLine = "#"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

integer :: Stream s m Char => ParsecT s u m Integer
integer = Tok.integer lexer

quotedString :: Stream s m Char => ParsecT s u m Text
quotedString = T.pack <$> Tok.stringLiteral lexer

charLit :: Stream s m Char => ParsecT s u m Char
charLit = Tok.charLiteral lexer

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = Tok.parens lexer

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = Tok.braces lexer

commaSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep = Tok.commaSep lexer

semiSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
semiSep = Tok.semiSep lexer

identifier :: Stream s m Char => ParsecT s u m Text
identifier = T.pack <$> Tok.identifier lexer

lowIdentifier :: Stream s m Char => ParsecT s u m Text
lowIdentifier = do
  x <- lower
  xs <- Tok.lexeme lexer $ many alphaNum
  return $ T.pack $ x:xs

capIdentifier :: Stream s m Char => ParsecT s u m Text
capIdentifier = do
  x <- upper
  xs <- Tok.lexeme lexer $ many alphaNum
  return $ T.pack $ x:xs

reserved :: Stream s m Char => String -> ParsecT s u m ()
reserved = Tok.reserved lexer

reservedOp :: Stream s m Char => String -> ParsecT s u m ()
reservedOp = Tok.reservedOp lexer

spaces :: Stream s m Char => ParsecT s u m ()
spaces = Tok.whiteSpace lexer

symbol :: Stream s m Char => String -> ParsecT s u m Text
symbol x = T.pack <$> Tok.symbol lexer x

operator :: Stream s m Char => ParsecT s u m Text
operator = T.pack <$> Tok.operator lexer

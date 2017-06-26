{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}


module Rlang.Lexer where

import Text.Parsec (ParsecT, Stream, (<|>))
import Text.Parsec.Char
import Text.Parsec.Text (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

myDef :: Stream s m Char => Tok.GenLanguageDef s u m
myDef = Tok.LanguageDef
               { Tok.commentStart   = ""
               , Tok.commentEnd     = ""
               , Tok.commentLine    = ""
               , Tok.nestedComments = True
               , Tok.identStart     = letter <|> char '_'
               , Tok.identLetter    = alphaNum <|> oneOf "_'"
               , Tok.opStart        = Tok.opLetter myDef
               , Tok.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , Tok.reservedOpNames= []
               , Tok.reservedNames  = []
               , Tok.caseSensitive  = True
               }

lexer :: Stream s m Char => Tok.GenTokenParser s u m
lexer = Tok.makeTokenParser style
  where
    ops = ["=","+","*","-",";"]
    names = ["def","extern"]
    style = myDef {
               Tok.commentLine = "#"
             , Tok.reservedOpNames = ops
             , Tok.reservedNames = names
             }

integer :: Stream s m Char => ParsecT s u m Integer
integer = Tok.integer lexer

quotedString :: Stream s m Char => ParsecT s u m String
quotedString = Tok.stringLiteral lexer

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = Tok.parens lexer

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = Tok.braces lexer

commaSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep = Tok.commaSep lexer

semiSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
semiSep = Tok.semiSep lexer

identifier :: Stream s m Char => ParsecT s u m String
identifier = Tok.identifier lexer

reserved :: Stream s m Char => String -> ParsecT s u m ()
reserved = Tok.reserved lexer

reservedOp :: Stream s m Char => String -> ParsecT s u m ()
reservedOp = Tok.reservedOp lexer

module Language.TheExperiment.Parser.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as T

import Control.Monad

eLanguageDef = T.LanguageDef
  { T.commentStart    = "/*"
  , T.commentEnd      = "*/"
  , T.commentLine     = "//"
  , T.nestedComments  = True
  , T.identStart      = letter <|> char '_'
  , T.identLetter     = alphaNum <|> char '_'
  , T.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , T.reservedOpNames = []
  , T.reservedNames   = ["type", "var", "foreign"]
  , T.caseSensitive   = True
  }

lexer :: T.TokenParser ()
lexer = T.makeTokenParser eLanguageDef

parens        = T.parens lexer
identifier    = T.identifier lexer
lexeme        = T.lexeme lexer
comma         = T.comma lexer
commaSep1     = T.commaSep1 lexer
symbol        = T.symbol lexer
reserved      = T.reserved lexer
stringLiteral = T.stringLiteral lexer

typeIdent  = lexeme $ do
  f <- oneOf ['A'..'Z']
  r <- many $ alphaNum <|> char '_'
  return $ f : r
varIdent   = lexeme $ do
  f <- oneOf $ '_' : ['a'..'z']
  r <- many $ alphaNum <|> char '_'
  return $ f : r
  
liftMp  f = liftM2 f getPosition
liftM2p f = liftM3 f getPosition
liftM3p f = liftM4 f getPosition

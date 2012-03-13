module Language.TheExperiment.Parser.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as T

import Control.Monad

opChars = oneOf ":!#$%&*+./<=>?@\\^|-~"

eLanguageDef = T.LanguageDef
  { T.commentStart    = "/*"
  , T.commentEnd      = "*/"
  , T.commentLine     = "//"
  , T.nestedComments  = True
  , T.identStart      = letter <|> char '_'
  , T.identLetter     = alphaNum <|> char '_'
  , T.opStart         = opChars
  , T.opLetter        = opChars
  , T.reservedOpNames = []
  , T.reservedNames   = ["type", "var", "foreign"]
  , T.caseSensitive   = True
  }

lexer :: T.TokenParser ()
lexer = T.makeTokenParser eLanguageDef

parens        = T.parens        lexer
identifier    = T.identifier    lexer
lexeme        = T.lexeme        lexer
comma         = T.comma         lexer
commaSep1     = T.commaSep1     lexer
symbol        = T.symbol        lexer
reserved      = T.reserved      lexer
reservedOp    = T.reservedOp    lexer
stringLiteral = T.stringLiteral lexer

typeIdent = T.identifier $ T.makeTokenParser 
          $ eLanguageDef { T.identStart = oneOf ['A'..'Z'] }
varIdent  = T.identifier $ T.makeTokenParser
          $ eLanguageDef { T.identStart = oneOf ['a'..'z'] }
  
liftMp  f = liftM2 f getPosition
liftM2p f = liftM3 f getPosition
liftM3p f = liftM4 f getPosition

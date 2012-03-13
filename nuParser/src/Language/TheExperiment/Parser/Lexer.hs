module Language.TheExperiment.Parser.Lexer where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T

import Control.Monad

opChars :: Monad m => ParsecT String u m Char
opChars = oneOf ":!#$%&*+./<=>?@\\^|-~"

typeIdent :: Parser String
typeIdent = T.identifier $ T.makeTokenParser 
          $ eLanguageDef { T.identStart = oneOf ['A'..'Z'] }

varIdent :: Parser String
varIdent  = T.identifier $ T.makeTokenParser
          $ eLanguageDef { T.identStart = oneOf ['a'..'z'] }
  
liftMp :: (SourcePos -> a -> b) -> Parser a -> Parser b
liftMp  f = liftM2 f getPosition

liftM2p :: (SourcePos -> a -> b -> c) -> Parser a -> Parser b -> Parser c
liftM2p f = liftM3 f getPosition

liftM3p :: (SourcePos -> a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
liftM3p f = liftM4 f getPosition

eLanguageDef :: T.LanguageDef a
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

parens :: Parser a -> Parser a
parens = T.parens lexer

identifier :: Parser String
identifier = T.identifier lexer

lexeme :: Parser a -> Parser a
lexeme = T.lexeme lexer

comma :: Parser String
comma = T.comma lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 lexer

symbol :: String -> Parser String
symbol = T.symbol lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

stringLiteral :: Parser String
stringLiteral = T.stringLiteral lexer

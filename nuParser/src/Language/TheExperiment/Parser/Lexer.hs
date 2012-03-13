module Language.TheExperiment.Parser.Lexer where

import Text.Parsec
import qualified Text.Parsec.Token as T

import Control.Monad
import qualified Control.Monad.State as S

type EParser a = ParsecT String () (S.State SourcePos) a

opChars :: Monad m => ParsecT String u m Char
opChars = oneOf ":!#$%&*+./<=>?@\\^|-~"

typeIdent :: EParser String
typeIdent = T.identifier $ T.makeTokenParser 
          $ eLanguageDef { T.identStart = oneOf ['A'..'Z'] }

varIdent :: EParser String
varIdent  = T.identifier $ T.makeTokenParser
          $ eLanguageDef { T.identStart = oneOf ['a'..'z'] }
  
liftMp :: (SourcePos -> a -> b) -> EParser a -> EParser b
liftMp  f = liftM2 f getPosition

liftM2p :: (SourcePos -> a -> b -> c) -> EParser a -> EParser b -> EParser c
liftM2p f = liftM3 f getPosition

liftM3p :: (SourcePos -> a -> b -> c -> d) -> EParser a -> EParser b -> EParser c -> EParser d
liftM3p f = liftM4 f getPosition

eLanguageDef :: Monad m => T.GenLanguageDef String u m
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

lexer :: Monad m => T.GenTokenParser String () m
lexer = T.makeTokenParser eLanguageDef

parens :: EParser a -> EParser a
parens = T.parens lexer

identifier :: EParser String
identifier = T.identifier lexer

lexeme :: EParser a -> EParser a
lexeme = T.lexeme lexer

comma :: EParser String
comma = T.comma lexer

commaSep1 :: EParser a -> EParser [a]
commaSep1 = T.commaSep1 lexer

symbol :: String -> EParser String
symbol = T.symbol lexer

reserved :: String -> EParser ()
reserved = T.reserved lexer

reservedOp :: String -> EParser ()
reservedOp = T.reservedOp lexer

stringLiteral :: EParser String
stringLiteral = T.stringLiteral lexer

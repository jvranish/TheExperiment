module Language.TheExperiment.Parser.Lexer where

import Data.Char

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T

import Control.Monad
import qualified Control.Monad.Trans.State as S

import Language.TheExperiment.Parser.AST.Expression

type ParserOperator = Operator String Operators (S.State SourcePos) (Expr ())
newtype Operators = Operators [(ParserOperator, Rational)]


type EParser a = ParsecT String Operators (S.State SourcePos) a

opChars :: Monad m => ParsecT String u m Char
opChars = oneOf ":!#$%&*+./<=>?@\\^|-~"

upperIdent :: EParser String
upperIdent = T.identifier $ T.makeTokenParser 
           $ eLanguageDef { T.identStart = oneOf ['A'..'Z'] }

lowerIdent :: EParser String
lowerIdent  = T.identifier $ T.makeTokenParser
            $ eLanguageDef { T.identStart = oneOf ['a'..'z'] }
  
liftMp :: (SourcePos -> () -> a -> b) -> EParser a -> EParser b
liftMp  f = liftM3 f getPosition (return ())

liftM2p :: (SourcePos -> () -> a -> b -> c) -> EParser a -> EParser b -> EParser c
liftM2p f = liftM4 f getPosition (return ())

liftM3p :: (SourcePos -> () -> a -> b -> c -> d) -> EParser a -> EParser b -> EParser c -> EParser d
liftM3p f = liftM5 f getPosition (return ())

liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6) }

liftM4p :: (SourcePos -> () -> a -> b -> c -> d -> e) -> EParser a -> EParser b -> EParser c -> EParser d -> EParser e
liftM4p f = liftM6 f getPosition (return ())

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
  , T.reservedNames   = ["block", "def", "if", "else", "elif", "type", "var", "while", "foreign", "return"]
  , T.caseSensitive   = True
  }

lexer :: Monad m => T.GenTokenParser String u m
lexer = T.makeTokenParser eLanguageDef

parens :: EParser a -> EParser a
parens = T.parens lexer

operator :: EParser String
operator = T.operator lexer

identifier :: EParser String
identifier = T.identifier lexer

lexeme :: EParser a -> EParser a
lexeme = T.lexeme lexer

comma :: EParser String
comma = T.comma lexer

whiteSpace :: EParser ()
whiteSpace = T.whiteSpace lexer

commaSep1 :: EParser a -> EParser [a]
commaSep1 = T.commaSep1 lexer

commaSep :: EParser a -> EParser [a]
commaSep = T.commaSep lexer

symbol :: String -> EParser String
symbol = T.symbol lexer

reserved :: String -> EParser ()
reserved = T.reserved lexer

reservedOp :: String -> EParser ()
reservedOp = T.reservedOp lexer

stringLiteral :: EParser String
stringLiteral = T.stringLiteral lexer

charLiteral :: EParser Char
charLiteral = T.charLiteral lexer

rational :: EParser Rational
rational = liftM (either toRational toRational) (T.naturalOrFloat lexer)

number :: Integer -> EParser Char -> EParser Integer 
number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }

float :: EParser Double
float = T.float lexer

decimal :: EParser Integer
decimal = T.decimal lexer

octal :: EParser Integer
octal = T.octal lexer

hexadecimal :: EParser Integer
hexadecimal = T.hexadecimal lexer

parseLex :: EParser a -> EParser a
parseLex p = do 
  whiteSpace
  x <- p
  eof
  return x




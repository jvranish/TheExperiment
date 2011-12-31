
module Language.TheExperiment.TempLexer  ( symbol
                                         , parens
                                         , brackets
                                         , natural
                                         , naturalOrFloat
                                         , rational
                                         , stringLiteral
                                         , operator
                                         , identifier
                                         , reserved
                                         , reservedOp
                                         , whiteSpace
                                         , comma
                                         , lexer
                                         ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

lexerStyle :: LanguageDef ()
lexerStyle = Token.LanguageDef
                { Token.commentStart   = "{-"
                , Token.commentEnd     = "-}"
                , Token.commentLine    = "//"
                , Token.nestedComments = True
                , Token.identStart     = letter
                , Token.identLetter    = alphaNum <|> oneOf "_'#" 
                , Token.opStart        = Token.opLetter lexerStyle
                , Token.opLetter       = oneOf "~!@$%^&*-+/?|=<>" 
                , Token.reservedOpNames= ["::"]
                , Token.reservedNames  = [ "return", "struct", "union"
                                         , "for", "while", "if", "else"
                                         , "do", "var", "foreign", "def"]
                , Token.caseSensitive  = True
                }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser lexerStyle



symbol :: String -> Parser String
symbol = Token.symbol lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer



natural :: Parser Integer
natural = Token.natural lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = Token.naturalOrFloat lexer

rational :: Parser Rational
rational = liftM (either toRational toRational) naturalOrFloat



stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer



operator :: Parser String
operator = Token.operator lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer



comma :: Parser String
comma = Token.comma lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

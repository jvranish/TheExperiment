module Language.TheExperiment.Lexer where

import Text.Parsec hiding (space)
import Text.Parsec.String

space :: Parser Char
space = char ' '


lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    return x
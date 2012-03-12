module Language.TheExperiment.Parser.Statement where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T

import Language.TheExperiment.Parser.Lexer

data Statement = Statement
  deriving (Show, Eq, Ord)

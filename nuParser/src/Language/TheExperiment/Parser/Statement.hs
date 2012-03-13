module Language.TheExperiment.Parser.Statement where

import Text.Parsec
-- import Text.Parsec.Pos
-- import Text.Parsec.String
import Text.Parsec.Indent
-- import qualified Text.Parsec.Token as T

-- import Language.TheExperiment.Parser.Lexer

data Statement = Statement
  deriving (Show, Eq, Ord)

type EIndentParser a = IndentParser String () a

preBlock :: EIndentParser String
preBlock = do
  s <- many1 alphaNum
  _ <- char '_'
  return s

inBlock :: EIndentParser String
inBlock = do
  s <- many1 alphaNum
  _ <- char ';'
  return s

aBlock :: EIndentParser (String, [String])
aBlock = withBlock (,) preBlock inBlock


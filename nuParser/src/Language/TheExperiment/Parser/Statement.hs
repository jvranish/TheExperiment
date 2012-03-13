module Language.TheExperiment.Parser.Statement where

import Control.Monad.Trans
import qualified Control.Monad.State as S

import Text.Parsec

-- import Text.Parsec.Pos
-- import Text.Parsec.String
import Text.Parsec.Indent
-- import qualified Text.Parsec.Token as T

-- import Language.TheExperiment.Parser.Lexer

data Statement = Statement
  deriving (Show, Eq, Ord)

preBlock :: ParsecT String u (S.State SourcePos) String
preBlock = do
  s <- many1 alphaNum
  _ <- char ':'
  spaces
  return s

inBlock :: ParsecT String u (S.State SourcePos) String
inBlock = do
  s <- many1 alphaNum
  _ <- char ';'
  spaces
  return s

aBlock :: ParsecT String u (S.State SourcePos) (String, [String])
aBlock = withBlock (,) preBlock inBlock

s = unlines [
  "foo:",
  "  bar;",
  "  baz;"
  ]

parseBlock = let name = "derp.e"
             in runIndent name $ runParserT aBlock () name s

module Language.TheExperiment.Parser.Statement where

import qualified Control.Monad.State as S

import Text.Parsec
import Text.Parsec.Indent

import Language.TheExperiment.Parser.Lexer

data Statement = Statement
  deriving (Show, Eq, Ord)

preBlock :: EParser String
preBlock = lexeme $ do
  s <- many1 alphaNum
  _ <- char ':'
  spaces
  return s

inBlock :: EParser String
inBlock = lexeme $ do
  s <- many1 alphaNum
  _ <- char ';'
  return s

aBlock :: ParsecT String () (S.State SourcePos) (String, [String])
aBlock = withBlock (,) preBlock inBlock

test_s :: String
test_s = unlines [
  "foo:",
  "  bar;",
  "  baz;"
  ]

parseBlock :: Either ParseError (String, [String])
parseBlock = let name = "derp.e"
             in runIndent name $ runParserT aBlock () name test_s

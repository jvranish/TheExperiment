module Language.TheExperiment.Parser 
  ( module Language.TheExperiment.Parser.Expression
  , module Language.TheExperiment.Parser.Lexer
  , module Language.TheExperiment.Parser.Literal
  , module Language.TheExperiment.Parser.Statement
  , runEParser
  ) where

import Text.Parsec
import Text.Parsec.Indent

import Language.TheExperiment.Parser.Expression
import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Literal
import Language.TheExperiment.Parser.Statement


runEParser :: String -> String -> EParser a -> Either ParseError a
runEParser filename s p = runIndent filename $ runParserT p () filename s 

--eParser :: String -> String -> Either ParseError Module
--eParser filename s = runIndent filename $ runParserT aModule () filename s

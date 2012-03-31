module Language.TheExperiment.Parser 
  ( module Language.TheExperiment.Parser.Expression
  , module Language.TheExperiment.Parser.Lexer
  , module Language.TheExperiment.Parser.Literal
  , module Language.TheExperiment.Parser.Statement
  , module Language.TheExperiment.Parser.Module
  , runEParser
  , parseFile
  ) where

import Text.Parsec
import Text.Parsec.Indent

import Language.TheExperiment.Parser.Expression
import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Literal
import Language.TheExperiment.Parser.Statement
import Language.TheExperiment.Parser.Module


runEParser :: String -> String -> EParser a -> Either ParseError a
runEParser filename s p = runIndent filename $ runParserT p (Operators []) filename s 

parseFile :: FilePath -> IO (Either ParseError ParsedModule)
parseFile filename  =  do 
  input <- readFile filename
  -- the seq is here because of the stupid lazy IO! >:|
  return $ seq (length input) $ runEParser filename input parseSource


--eParser :: String -> String -> Either ParseError Module
--eParser filename s = runIndent filename $ runParserT aModule () filename s

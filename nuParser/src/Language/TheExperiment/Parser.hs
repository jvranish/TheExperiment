module Language.TheExperiment.Parser (
  eParser
) where

import Text.Parsec
import Text.Parsec.Indent
import Language.TheExperiment.Parser.Definition

eParser :: String -> Either ParseError Definition
eParser s = let name = "derp.e"
            in runIndent name $ runParserT aDefinition () name s

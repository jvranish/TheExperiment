module Language.TheExperiment.Parser (
  eParser
) where

import Text.Parsec
import Language.TheExperiment.Parser.Definition

eParser :: String -> Either ParseError Definition
eParser s = parse aDefinition "derp.e" s

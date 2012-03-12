module Language.TheExperiment.Parser (
  eParser
) where

import Text.Parsec
import Language.TheExperiment.Parser.Definition

-- eParser :: Parser Definition
eParser s = parse aDefinition "derp.e" s

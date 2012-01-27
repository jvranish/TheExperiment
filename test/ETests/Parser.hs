module ETests.Parser where

import Language.TheExperiment.AST
import Language.TheExperiment.Parser

test = do
  map check parse_tests

parse_tests = [ ("\"Hello World\"", StringLiteral "Hello World")
              , ("\'C\'"          , CharLiteral 'C')
              , ("0b110011"       , BinLiteral 51)
              , ("0x12345678"     , HexLiteral 305419896)
              , ("123.43435342"   , FloatLiteral "123.43435342" 123.43435342)
              ]

check (input, expected) = (comparison, input, expected, actual)
  where
    actual = parseExpr input
    comparison = actual == expected

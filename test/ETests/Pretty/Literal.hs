module ETests.Pretty.Literal 
  ( prettyLiteralSpecs
  , prettyLiteralTestCases
  ) where

import Text.Parsec

import Text.PrettyPrint.HughesPJ

import Test.Hspec
import Test.Hspec.HUnit()

import Language.TheExperiment.Parser
import Language.TheExperiment.Parser.Expression
import Language.TheExperiment.Pretty.Literal

import ETests.Utils

prettyLiteralTestCases :: (String -> String -> IO ()) -> [Specs]
prettyLiteralTestCases prettyFrom = 
  [ it "pretty prints a zero integer literal" $
      "0" `prettyFrom` "0"
  , it "pretty prints an integer literal" $
      "1234" `prettyFrom` "1234"
  , it "pretty prints a hex literal`" $
      "0xAF" `prettyFrom` "0xAF"
  , it "pretty prints an octal literal" $
      "0o777" `prettyFrom` "0o777"
  , it "pretty prints a binary literal" $
      "0b1010" `prettyFrom` "0b1010"
  , it "pretty prints a char literal" $
      "'j'" `prettyFrom` "'j'"
  , it "pretty prints a string literal" $
      "\"asdf\"" `prettyFrom` "\"asdf\""
  , it "pretty prints a float literal" $
      "3.14159" `prettyFrom` "3.14159"
  ]

prettyLiteralSpecs :: Specs
prettyLiteralSpecs = describe "prettyLiteral" $ prettyLiteralTestCases prettyFrom
  where
    prettyFrom expected input = 
        case runEParser "tests" input (parseLex aLiteral) of
          Right result -> eTestAssertEqual "prettyType" 
                            expected
                            (render $ prettyLiteral result)
          Left e -> error $ "fix your stupid test: " ++ show e
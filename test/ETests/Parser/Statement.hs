module ETests.Parser.Statement where

import Control.Applicative

import Test.Hspec

import ETests.Utils

import Text.Parsec
import Text.Parsec.Error

import Language.TheExperiment.AST
import Language.TheExperiment.Parser
import Language.TheExperiment.Parser.Statement

statementSpecs :: Specs
statementSpecs = describe "aStatement" (statementTestCases parsesTo)
  where
    parsesTo input expected = eTestParse "aStmt" expected (runEParser "tests" input (aStatement <* eof))

statementTestCases :: (String -> Either [Message] ParsedStatement -> IO ()) -> [Specs]
statementTestCases parsesTo = 
  [ it "parses a `return` statement" $
      "return 9" `parsesTo` (Right $ pReturn "9")
  ]

pReturn expr = Return blankPos () (Literal blankPos () (IntegerLiteral 9))

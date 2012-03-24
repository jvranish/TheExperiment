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
  [ it "parses a return statement" $
      "return 9" `parsesTo` (Right pReturn)
  , it "parses an assignment" $
      "foo = 9" `parsesTo` (Right pAssign)
  , it "parses a call" $
      "bar(9, foo)" `parsesTo` (Right pCallStmt)
  ]

pReturn :: ParsedStatement
pReturn = Return blankPos () (Literal blankPos () (IntegerLiteral 9))

pAssign :: ParsedStatement
pAssign = Assign blankPos () "foo" (Literal blankPos () $ IntegerLiteral 9)

pCallStmt :: ParsedStatement
pCallStmt = CallStmt blankPos () call
  where
    call   = Call blankPos () func params
    func   = Identifier blankPos () "bar" NotOperator
    params = [ Literal blankPos () $ IntegerLiteral 9
             , Identifier blankPos () "foo" NotOperator
             ]

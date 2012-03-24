module ETests.Parser.Statement where

import Control.Applicative

import Test.Hspec

import ETests.Utils

import Text.Parsec
import Text.Parsec.Indent
import Text.Parsec.Error

import Language.TheExperiment.AST
import Language.TheExperiment.Parser
import Language.TheExperiment.Parser.Module
import Language.TheExperiment.Parser.Statement

statementSpecs :: Specs
statementSpecs = describe "aStatement" (statementTestCases parsesTo)
  where
    parsesTo input expected = eTestParse "aStmt" expected (runEParser "tests" input aStatement)

statementTestCases :: (String -> Either [Message] ParsedStatement -> IO ()) -> [Specs]
statementTestCases parsesTo = 
  [ it "parses a return statement" $
      "return 9" `parsesTo` (Right pReturn)
  , it "parses an assignment" $
      "foo = 9" `parsesTo` (Right pAssign)
  , it "parses a call" $
      "bar(9, foo)" `parsesTo` (Right pCallStmt)
  , it "parses a block" $
      unlines [ "block: foo = 9"
              , "       bar = 10"
              , "       baz = foo"
              ] `parsesTo` (Right pBlock)
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

pBlock :: ParsedStatement
pBlock = Block blankPos () rawBlock
  where
    rawBlock = RawBlock blankPos () [s1, s2, s3]
    s1 = Stmt $ Assign blankPos () "foo" (Literal blankPos () $ IntegerLiteral 9)
    s2 = Stmt $ Assign blankPos () "bar" (Literal blankPos () $ IntegerLiteral 10)
    s3 = Stmt $ Assign blankPos () "baz" (Identifier blankPos () "foo" NotOperator)

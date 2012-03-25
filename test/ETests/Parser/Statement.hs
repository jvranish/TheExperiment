module ETests.Parser.Statement where

import Control.Applicative

import Test.Hspec
import Test.Hspec.HUnit

import ETests.Utils

import Text.Parsec
import Text.Parsec.Indent
import Text.Parsec.Error

import Language.TheExperiment.AST
import Language.TheExperiment.Parser
import Language.TheExperiment.Parser.Module
import Language.TheExperiment.Parser.Statement
import ETests.Parser.Expression

statementSpecs :: Specs
statementSpecs = describe "aStatement" (statementTestCases parsesTo)
  where
    parsesTo input expected = eTestParse "aStmt" expected (runEParser "tests" input aStatement)

statementTestCases :: (String -> Either [Message] ParsedStatement -> IO ()) -> [Specs]
statementTestCases parsesTo = 
  [ it "parses a return statement" $
      "return 9" `parsesTo` (Right returnExample)
  , it "parses an assignment" $
      "foo = 9" `parsesTo` (Right assignExample)
  , it "parses a call" $
      "bar(9, foo)" `parsesTo` (Right callExample)
  , it "parses a block" $
      unlines [ "block: foo = 9"
              , "       bar = 10"
              , "       baz = foo"
              ] `parsesTo` (Right blockExample)
  , it "parses an 'if'" $ -- how do we want to handle elif?
      unlines [ "if boop:"
              , "  x = 1"
              , "else:"
              , "  x = 2"
              ] `parsesTo` (Right ifExample)
  ]

returnExample :: ParsedStatement
returnExample = pReturn $ pLiteral $ IntegerLiteral 9

assignExample :: ParsedStatement
assignExample = pAssign "foo" $ pLiteral $ IntegerLiteral 9

callExample :: ParsedStatement
callExample = pCallStmt call
  where
    call   = pCall func params
    func   = pIdentifier "bar"
    params = [ pLiteral $ IntegerLiteral 9
             , pIdentifier "foo"
             ]

blockExample :: ParsedStatement
blockExample = pBlock rawBlock
  where
    rawBlock = pRawBlock [s1, s2, s3]
    s1 = Stmt $ pAssign "foo" (pLiteral $ IntegerLiteral 9)
    s2 = Stmt $ pAssign "bar" (pLiteral $ IntegerLiteral 10)
    s3 = Stmt $ pAssign "baz" (pIdentifier "foo")

ifExample :: ParsedStatement
ifExample = pIf cond ifThen (Just ifElse)
  where
    cond   = pIdentifier "boop"
    ifThen = pRawBlock [ Stmt $ pAssign "x" (pLiteral $ IntegerLiteral 1)]
    ifElse = pRawBlock [ Stmt $ pAssign "x" (pLiteral $ IntegerLiteral 2)]



pReturn :: ParsedExpr -> ParsedStatement
pReturn expr = Return blankPos () expr

pAssign :: String -> ParsedExpr -> ParsedStatement
pAssign name expr = Assign blankPos () name expr

pCallStmt :: ParsedExpr -> ParsedStatement
pCallStmt expr = CallStmt blankPos () expr

pRawBlock :: [DefOrStatement ()] -> ParsedRawBlock
pRawBlock xs = RawBlock blankPos () xs

pBlock :: ParsedRawBlock -> ParsedStatement
pBlock rawBlock = Block blankPos () rawBlock

pIf :: ParsedExpr -> ParsedRawBlock -> Maybe (ParsedRawBlock) -> ParsedStatement
pIf cond ifThen ifElse = If blankPos () cond ifThen ifElse


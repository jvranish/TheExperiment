module ETests.Parser.Statement where

-- import Control.Applicative

import Test.Hspec
-- import Test.Hspec.HUnit

import ETests.Utils

-- import Text.Parsec
-- import Text.Parsec.Indent
import Text.Parsec.Error

import Language.TheExperiment.Parser.AST
import Language.TheExperiment.Parser
-- import Language.TheExperiment.Parser.Module
-- import Language.TheExperiment.Parser.Statement
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
  , it "parses an `if...`" $ 
      unlines [ "if boop:"
              , "  x = 1"
              ] `parsesTo` (Right ifExample)
  , it "parses an `if...else...`" $ -- how do we want to handle elif?
      unlines [ "if boop:"
              , "  x = 1"
              , "else:"
              , "  x = 2"
              ] `parsesTo` (Right ifElseExample)
  , it "parses an `if...elsif...`" $
      unlines [ "if boop:"
              , "  x = 1"
              , "elif bap:"
              , "  x = 2"
              ] `parsesTo` (Right ifElifExample)
  , it "parses an `if...elsif...else...`" $
      unlines [ "if boop:"
              , "  x = 1"
              , "elif bap:"
              , "  x = 2"
              , "else:"
              , "  x = 3"
              ] `parsesTo` (Right ifElifElseExample)
  , it "parses a `while ...`" $
    unlines [ "while zoop:"
            , "  x = 1"
            , "  y = 1"
            ] `parsesTo` (Right whileExample)
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
blockExample = pBlock [s1, s2, s3]
  where
    s1 = Stmt $ pAssign "foo" (pLiteral $ IntegerLiteral 9)
    s2 = Stmt $ pAssign "bar" (pLiteral $ IntegerLiteral 10)
    s3 = Stmt $ pAssign "baz" (pIdentifier "foo")

ifExample :: ParsedStatement
ifExample = pIf cond thenBlock Nothing
  where
    cond      = pIdentifier "boop"
    thenBlock = pBlock [ Stmt $ pAssign "x" lit1 ]
    lit1      = pLiteral $ IntegerLiteral 1

ifElseExample :: ParsedStatement
ifElseExample = pIf cond thenBlock (Just elseBlock)
  where
    cond      = pIdentifier "boop"
    thenBlock = pBlock [ Stmt $ pAssign "x" lit1 ]
    elseBlock = pBlock [ Stmt $ pAssign "x" lit2 ]
    lit1      = pLiteral $ IntegerLiteral 1
    lit2      = pLiteral $ IntegerLiteral 2

ifElifExample :: ParsedStatement
ifElifExample = pIf cond thenBlock (Just elifBlock)
  where
    cond      = pIdentifier "boop"
    thenBlock = pBlock [ Stmt $ pAssign "x" lit1 ]
    elifBlock = pIf
                  (pIdentifier "bap")
                  (pBlock [ Stmt $ pAssign "x" lit2 ])
                  Nothing
    lit1 = pLiteral $ IntegerLiteral 1
    lit2 = pLiteral $ IntegerLiteral 2

ifElifElseExample :: ParsedStatement
ifElifElseExample = pIf cond thenBlock (Just elifBlock)
  where
    cond      = pIdentifier "boop"
    thenBlock = pBlock [ Stmt $ pAssign "x" lit1 ]
    elifBlock = pIf
                  (pIdentifier "bap")
                  (pBlock [ Stmt $ pAssign "x" lit2 ])
                  (Just elseBlock)
    elseBlock = pBlock [ Stmt $ pAssign "x" lit3 ]
    lit1 = pLiteral $ IntegerLiteral 1
    lit2 = pLiteral $ IntegerLiteral 2
    lit3 = pLiteral $ IntegerLiteral 3

whileExample :: ParsedStatement
whileExample = While blankPos () cond whileBlock
  where
    cond = pIdentifier "zoop"
    whileBlock = pBlock [ Stmt $ pAssign "x" lit1
                                      , Stmt $ pAssign "y" lit1
                                      ]
    lit1 = pLiteral $ IntegerLiteral 1
    
pIf :: ParsedExpr -> ParsedStatement -> Maybe ParsedStatement -> ParsedStatement
pIf cond thenBlock elseBlock = If blankPos () cond thenBlock elseBlock  

pReturn :: ParsedExpr -> ParsedStatement
pReturn expr = Return blankPos () expr

pAssign :: String -> ParsedExpr -> ParsedStatement
pAssign name expr = Assign blankPos () name expr

pCallStmt :: ParsedExpr -> ParsedStatement
pCallStmt expr = CallStmt blankPos () expr

pBlock :: [DefOrStatement ()] -> ParsedStatement
pBlock xs = Block blankPos () xs

module ETests.Pretty.Statement
  ( prettyStatementSpecs
  ) where

import Text.Parsec

import Text.PrettyPrint.HughesPJ

import Test.Hspec
import Test.Hspec.HUnit()

import Language.TheExperiment.Parser
import Language.TheExperiment.Pretty.Statement

import ETests.Parser.Expression
import ETests.Utils


prettyStatementTestCases :: (String -> String -> IO ()) -> [Specs]
prettyStatementTestCases prettyFrom =
  [ it "pretty prints an assignment statement" $
    "a = 9" `prettyFrom` "a = 9"
  ]

prettyStatementSpecs = describe "prettyStatement" $ prettyStatementTestCases prettyForm
  where
    prettyForm expected input = 
        case runEParser "tests" (opDefs ++ input) (parseLex $ many anOpDef >>= putState . Operators >> aStatement) of
          Right result -> eTestAssertEqual "prettyStatement" 
                            expected
                            (render $ prettyStatement result)
          Left e -> error $ "fix our stupid test: " ++ show e

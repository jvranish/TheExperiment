module ETests.Pretty.Expression 
  ( prettyExpressionSpecs
  ) where

import Text.Parsec

import Text.PrettyPrint.HughesPJ

import Test.Hspec
import Test.Hspec.HUnit()

import Language.TheExperiment.Parser
import Language.TheExperiment.Parser.Expression
import Language.TheExperiment.Pretty.Expression

import ETests.Parser.Expression
import ETests.Utils

prettyExpressionTestCases :: (String -> String -> IO ()) -> [Specs]
prettyExpressionTestCases prettyFrom = 
  [ it "pretty prints a variable" $
      "a" `prettyFrom` "a"
  , it "pretty prints basic call" $
      "foo(a, b)" `prettyFrom` "foo(a, b)"
  , it "pretty prints numeric literal" $
      pending "not implemented yet"
      --"1234" `prettyFrom` "1234"
  , it "pretty prints operators (with correct precedence and assoc)" $
      pending "not implemented yet"
  ]

prettyExpressionSpecs :: Specs
prettyExpressionSpecs = describe "prettyExpression" $ prettyExpressionTestCases prettyFrom
  where
    prettyFrom expected input = 
        case runEParser "tests" (opDefs ++ input) (parseLex $ many anOpDef >>= putState . Operators >> anExpr) of
          Right result -> eTestAssertEqual "prettyType" 
                            expected
                            (render $ prettyExpression result)
          Left e -> error $ "fix your stupid test: " ++ show e
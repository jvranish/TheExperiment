module ETests.Pretty.Expression 
  ( prettyExpressionSpecs
  ) where

import Text.Parsec

import Text.PrettyPrint.HughesPJ

import Test.Hspec
import Test.Hspec.HUnit()

import Language.TheExperiment.Parser
import Language.TheExperiment.Pretty.Expression

import ETests.Pretty.Literal
import ETests.Parser.Expression
import ETests.Utils

prettyExpressionTestCases :: (String -> String -> IO ()) -> [Specs]
prettyExpressionTestCases prettyFrom = 
  prettyLiteralTestCases prettyFrom ++ 
  [ it "pretty prints a variable" $
      "a" `prettyFrom` "a"
  , it "pretty prints basic call" $
      "foo(a, b)" `prettyFrom` "foo(a, b)"
  , it "pretty prints numeric literal" $
      "1234" `prettyFrom` "1234"
  , it "pretty prints operators (with correct precedence)" $
      "a + b * c" `prettyFrom` "a + (b * c)"
  , it "pretty prints operators adding parens as needed" $
      "(a + b) * c" `prettyFrom` "(a + b) * c"
  , it "pretty prints left assoc operator adding parens as needed" $
      "a + (b + c)" `prettyFrom` "a + (b + c)"
  , it "pretty prints infix left assoc operator with parens" $
      "(a + b) + c" `prettyFrom` "a + b + c"
  , it "pretty prints left assoc operator without parens if not needed" $
      pending "not sure if we really need this"
  , it "pretty prints prefix with infix" $
      "a + -b" `prettyFrom` "a + (-b)"
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
module ETests.Parser.Expression
  ( anExprSpecs
  , anExprTestCases
  ) where


import Control.Applicative

import Text.Parsec
import Text.Parsec.Error

import Test.Hspec

import Language.TheExperiment.AST
import Language.TheExperiment.Parser.Expression
import Language.TheExperiment.Parser


import ETests.Parser.Literal
import ETests.Utils


--data Expr a
--        = Call       { exprPos      :: SourcePos
--                     , exprNodeData :: a
--                     , callFunc     :: Expr a
--                     , callParams   :: [Expr a]
--                     }
--        | Identifier { exprPos      :: SourcePos
--                     , exprNodeData :: a
--                     , idName       :: String
--                     , opFormat     :: OpFormat
--                     }
--        | Literal    { exprPos      :: SourcePos
--                     , exprNodeData :: a
--                     , literal      :: Literal
--                     }


--foo(a, b, c)

anExprSpecs :: Specs
anExprSpecs = describe "anExpr" (anExprTestCases parsesTo)
  where
    parsesTo input expected = eTestParse "anExpr" expected (runEParser "tests" input (anExpr <* eof))

opDefs :: String
opDefs = "infixl + add 4\
        \ infixl - sub 4\
        \ infixl * mul 8\
        \ infixl / div 8\
        \ infixl == eq 1 "

anExprTestCases :: (String -> Either [Message] ParsedExpr -> IO ()) -> [Specs]
anExprTestCases parsesTo = 
  aLiteralTestCases (\i e -> parsesTo i (fmap pLiteral e)) ++
  [ it "parses an identifier" $
      "a" `parsesTo` (Right $ pIdentifier "a")
  , it "parses an identifier" $
      pending "add quickcheck property for this test"
  , it "parses an identifier with trailing uppercase chars" $
      "aFoo" `parsesTo` (Right $ pIdentifier "aFoo")
  , it "fails to parse an identifier starting with uppercase chars" $
      --"Foo" `parsesTo` (Left [])
      pending "improve the eTestParse function so that it does some\
             \ filtering or something"
  , it "parses basic call" $
      "foo(a, b)" `parsesTo` (Right $ pCall (pIdentifier "foo") 
                                        [ pIdentifier "a"
                                        , pIdentifier "b"])
  , it "parses a basic left assoc infix operator" $
      (opDefs ++ "a + b") `parsesTo` (Right $ pCall (pOperator "+" $ InL 4) 
                                                [ pIdentifier "a"
                                                , pIdentifier "b"])
  ]

pOperator :: String -> OpFormat -> ParsedExpr
pOperator name fmt = Identifier blankPos () name fmt

pIdentifier :: String -> ParsedExpr
pIdentifier name = Identifier blankPos () name NotOperator

pLiteral :: Literal -> ParsedExpr
pLiteral lit = Literal blankPos () lit

pCall :: ParsedExpr -> [ParsedExpr] -> ParsedExpr
pCall f params = Call blankPos () f params


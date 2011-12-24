module Language.TheExperiment.CodeGenSpec (main) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Language.C.Pretty

import Language.TheExperiment.AST
import Language.TheExperiment.CodeGen

main :: IO Bool
main = hspecB $ concat $ [ genExprSpecs
                         ]

genExprSpecs :: Specs
genExprSpecs = describe "genExpr" [
    -- I expect this property not to hold for large integers
    it "generates a C integer from an integer literal in the expression tree" (property $ \x -> show (genExpr (ExprLit (LiteralInt x))) == show x),
    -- TODO # we should added checks that it only allows integers that fit the required type
    it "generates a variable from a variable in the expression tree" (property $ \x -> show (genExpr (Identifier x)) == x),
    --it "generates a function call from a call in the expression tree" (property $ )
    ]


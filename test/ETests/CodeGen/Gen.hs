module ETests.CodeGen.Gen
  ( genExprSpecs
  ) where

import Test.Hspec
import Test.Hspec.HUnit()

genExprSpecs :: Specs
genExprSpecs = describe "genExpr" $ 
  [ it "Generates code for a binary literal" $
      pending "Language.C does not currently support binary literals"
  , it "works" $ 
      pending "add more test cases!"
  ]
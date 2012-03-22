module ETests.Pretty.Type 
  ( testPrettyTypeSignature
  , testPrettyType
  , prettyTypeSignatureSpecs
  , prettyTypeSpecs 
  ) where

import Control.Applicative

import Text.Parsec

import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.Parser
import Language.TheExperiment.Parser.Type
import Language.TheExperiment.Pretty.Type

import ETests.Utils

import Test.Hspec
import Test.Hspec.HUnit()

testPrettyTypeSignature :: IO Specs
testPrettyTypeSignature = hspec prettyTypeSignatureSpecs

prettyTypeSignatureSpecs :: Specs
prettyTypeSignatureSpecs = describe "prettyTypeSignature" $ 
  prettyTypeTestCases prettyFrom ++ 
  [ it "pretty prints a type variable with constraints" $
      "a : Int32 | UInt32 => a" `prettyFrom` "a : Int32 | UInt32 => a"
  , it "pretty prints a function type with constraints" $
      "a : Float | Double => a, a -> a" `prettyFrom` 
      "a : Float | Double => a, a -> a"
  , it "pretty prints a function type with multiple constraints" $
      "a : Float | Double, b : Int32 | UInt32 => a, a -> b" `prettyFrom` 
      "a : Float | Double, b : Int32 | UInt32 => a, a -> b"
  ]
  where
    prettyFrom expected input = 
        case runEParser "tests" input (aTypeSignature <* eof) of
          Right result -> eTestAssertEqual "prettyTypeSignature" 
                            expected
                            (render $ prettyTypeSignature result)
          Left e -> error $ "fix your stupid test: " ++ show e

testPrettyType :: IO Specs
testPrettyType = hspec prettyTypeSpecs

prettyTypeTestCases :: (String -> String -> IO ()) -> [Specs]
prettyTypeTestCases prettyFrom = 
  [ it "pretty prints a type variable" $
      "a" `prettyFrom` "a"
  , it "pretty prints a type name" $
      "Foo" `prettyFrom` "Foo"
  , it "pretty prints a type function" $
      "a, X, c -> c" `prettyFrom` "a, X, c -> c"
  , it "pretty prints a function with no parameters" $
      "-> Foo" `prettyFrom` "-> Foo"
  , it "strips off unnecessary parens" $
      "-> Foo" `prettyFrom` "(-> Foo)"
  , it "doesn't strip off necessary parens" $
      "a, (-> Int32) -> b" `prettyFrom` "a, (-> Int32) -> b"
  , it "doesn't strip off necessary parens in function type" $
      "a, Foo (f a) Int32 -> b" `prettyFrom` "a, Foo (f a) Int32 -> b"
  , it "doesn't strip off necessary parens in function type" $
      "a, Foo (a -> b) Int32 -> b" `prettyFrom` "a, Foo (a -> b) Int32 -> b"
  , it "doesn't strip off necessary parens in type call" $
      "Foo (Boo a) Int32" `prettyFrom` "Foo (Boo a) Int32"
  , it "works in weird cases" $
      "a -> (-> b)" `prettyFrom` "a -> (-> b)"
  ]

prettyTypeSpecs :: Specs
prettyTypeSpecs = describe "prettyType" $ prettyTypeTestCases prettyFrom
  where
    prettyFrom expected input = 
        case runEParser "tests" input (aType <* eof) of
          Right result -> eTestAssertEqual "prettyType" 
                            expected
                            (render $ prettyType result)
          Left e -> error $ "fix your stupid test: " ++ show e


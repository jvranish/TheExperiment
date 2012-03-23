module ETests.Utils where

import Control.Monad (unless)
import Text.Parsec.Pos
import Text.Parsec.Error

import Language.TheExperiment.AST.Type
import Language.TheExperiment.AST.Expression

import Test.HUnit


class (Eq a) => TestComp a where
  testComp :: a -> a -> Bool
  testComp a b = a == b

instance (TestComp a) => TestComp [a] where
  testComp a b = and $ zipWith testComp a b

instance TestComp Char

instance TestComp ParsedType where
  testComp (TypeName _ n) (TypeName _ n') = n `testComp` n'
  testComp (TypeVariable _ v) (TypeVariable _ v') = v `testComp` v'
  testComp (TypeCall _ f p) (TypeCall _ f' p') = (f `testComp` f') && (p `testComp` p')
  testComp (FunctionType _ a r) (FunctionType _ a' r') = (a `testComp` a') && (r `testComp` r')
  testComp _ _ = False

instance TestComp TypeSignature where
  testComp (TypeSignature as a) (TypeSignature bs b) = testComp as bs && testComp a b

instance TestComp TypeConstraint where
  testComp (TypeConstraint a as) (TypeConstraint b bs) = a == b && testComp as bs


instance TestComp Literal
instance (TestComp a, TestComp b) => TestComp (Either a b)
instance TestComp Message

instance Show Message where
  show (SysUnExpect s) = "SysUnExpect " ++ s
  show (UnExpect s) = "UnExpect " ++ s
  show (Expect s) = "Expect " ++ s
  show (Message s) = "Message " ++ s

eTestAssertEqual :: (TestComp a, Show a) => String -> a -> a -> Assertion
eTestAssertEqual preface expected actual =
  unless (actual `testComp` expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right a) = Right a

eTestParse :: (TestComp a, Show a) => String -> Either [Message] a -> Either ParseError a -> Assertion
eTestParse preface expected actual =
  unless ((mapLeft errorMessages actual) `testComp` expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show (mapLeft errorMessages actual) --actual

blankPos :: SourcePos
blankPos = initialPos "tests"

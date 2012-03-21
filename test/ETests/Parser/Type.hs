module ETests.Parser.Type 
  ( testParseTypeSignature
  , testParseType
  ) where

import Control.Applicative

import Text.Parsec
import Text.Parsec.Pos

import Language.TheExperiment.AST.Type
import Language.TheExperiment.Parser
import Language.TheExperiment.Parser.Type

import ETests.Utils

import Test.Hspec
import Test.Hspec.HUnit()
import Test.HUnit


testParseTypeSignature :: IO Specs
testParseTypeSignature = hspec aTypeSignatureSpecs

aTypeSignatureSpecs :: Specs
aTypeSignatureSpecs = describe "aTypeSignature" $
    aTypeTestCases parsesToSig ++ 
    [ it "Parses a plain type " $
        "a" `parsesTo` (TypeSignature [] $ pTypeVariable "a")
    , it "Parses a type variable with a constraint" $
        "a : UInt32 | Int32 => a" `parsesTo` 
        (TypeSignature 
            [TypeConstraint "a" 
              [pTypeName "UInt32", pTypeName "Int32"]
            ] 
            (pTypeVariable "a")
        )
    , it "Parses a function type with a constraint" $
        "a : UInt32 | Int32 => a, a -> a" `parsesTo` 
        (TypeSignature 
            [TypeConstraint "a" 
              [pTypeName "UInt32", pTypeName "Int32"]
            ] 
            ( pFunctionType 
                [ pTypeVariable "a"
                , pTypeVariable "a"]
                (pTypeVariable "a") )
        )
    , it "Parses a function type with a couple constraints" $
        "a : UInt32 | Int32, b : Float | Double => a, b -> b" `parsesTo` 
        (TypeSignature 
            [TypeConstraint "a" 
              [pTypeName "UInt32", pTypeName "Int32"]
            , TypeConstraint "b" 
              [pTypeName "Float", pTypeName "Double"]
            ] 
            ( pFunctionType 
                [ pTypeVariable "a"
                , pTypeVariable "b"]
                (pTypeVariable "b") )
        )
    ]
  where
    parsesToSig input expected = parsesTo input (TypeSignature [] expected)
    parsesTo = runTestCase "aTypeSignature" aTypeSignature



testParseType :: IO Specs
testParseType = hspec aTypeSpecs

aTypeSpecs :: Specs
aTypeSpecs = describe "aType" $ aTypeTestCases parsesTo
  where
    parsesTo = runTestCase "aType" aType

aTypeTestCases :: (String -> ParsedType -> IO ()) -> [Specs]  
aTypeTestCases parsesTo = 
  [ it "Parses a TypeName" $
     "UInt32" `parsesTo` (pTypeName "UInt32")
  , it "Parses a TypeName in parens" $
     "(UInt32)" `parsesTo` (pTypeName "UInt32")
  , it "Parses a TypeVariable" $
      "a" `parsesTo` (pTypeVariable "a")
  , it "Parses a FunctionType" $
      "A, B -> C" `parsesTo` 
        (pFunctionType [(pTypeName "A"), (pTypeName "B")] (pTypeName "C"))
  , it "Parses a FunctionType with type variables" $
      "a, b -> c" `parsesTo` 
        (pFunctionType 
          [(pTypeVariable "a"), (pTypeVariable "b")] (pTypeVariable "c"))
  , it "Parses a FunctionType" $
      "A -> C" `parsesTo` 
        (pFunctionType [pTypeName "A"] (pTypeName "C"))
  , it "Parses a FunctionType" $
      "-> C" `parsesTo` 
        (pFunctionType [] (pTypeName "C"))
  , it "Parses a TypeCall" $
      "F a" `parsesTo` 
        (pTypeCall (pTypeName "F") [pTypeVariable "a"])
  , it "Parses a TypeCall with a function parameter" $
      "F (-> C)" `parsesTo` 
        (pTypeCall (pTypeName "F") [pFunctionType [] (pTypeName "C")])
  , it "Parses a FunctionType with a FunctionType argument " $
      "Foo a, (a -> b) -> c" `parsesTo`
        (pFunctionType 
            [ pTypeCall (pTypeName "Foo") [pTypeVariable "a"]
            , pFunctionType [pTypeVariable "a"] (pTypeVariable "b")
            ]
            (pTypeVariable "c"))
  ]

pTypeName :: String -> ParsedType
pTypeName name = TypeName blankPos name

pTypeVariable :: String -> ParsedType
pTypeVariable name = TypeVariable blankPos name

pFunctionType :: [ParsedType] -> ParsedType -> ParsedType
pFunctionType params ret = FunctionType blankPos params ret

pTypeCall :: ParsedType -> [ParsedType] -> ParsedType
pTypeCall typeFunc args = TypeCall blankPos typeFunc args

        
runTestCase :: (Show a, TestComp a)
            => String -> EParser a -> String -> a -> IO ()
runTestCase name p text expected= do
  case runEParser "tests" text (p <* eof) of
    Left e -> error $ "Parse failed: " ++ show e
    Right x -> eTestAssertEqual name expected x




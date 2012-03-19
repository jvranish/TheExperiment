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
    parsesToSig input expected = runTestCase "aTypeSignature" aTypeSignature input (Just $ TypeSignature [] expected)
    parsesTo input expected = runTestCase "aTypeSignature" aTypeSignature input (Just $ expected)



testParseType :: IO Specs
testParseType = hspec aTypeSpecs

aTypeSpecs :: Specs
aTypeSpecs = describe "aType" $ aTypeTestCases parsesTo
  where
    parsesTo input expected = runTestCase "aType" aType input (Just $ expected)

aTypeTestCases :: (String -> ParsedType -> IO ()) -> [Specs]  
aTypeTestCases parsesTo = 
  [ it "Parses a TypeName" $
     "UInt32" `parsesTo` (pTypeName "UInt32")
  , it "Parses a TypeVariable" $
      "a" `parsesTo` (pTypeVariable "a")
  , it "Parses a FunctionType" $
      "A, B -> C" `parsesTo` 
        (pFunctionType [(pTypeName "A"), (pTypeName "B")] (pTypeName "C"))
  , it "Parses a FunctionType" $
      "A -> C" `parsesTo` 
        (pFunctionType [pTypeName "A"] (pTypeName "C"))
  , it "Parses a FunctionType" $
      "-> C" `parsesTo` 
        (pFunctionType [] (pTypeName "C"))
  , it "Parses a TypeCall" $
      "F (-> C)" `parsesTo` 
        (pTypeCall (pTypeName "F") [pFunctionType [] (pTypeName "C")])
  ]
    


pTypeName :: String -> ParsedType
pTypeName name = ParsedType blankPos $ TypeName name

pTypeVariable :: String -> ParsedType
pTypeVariable name = ParsedType blankPos $ TypeVariable (Just name) []

pFunctionType :: [ParsedType] -> ParsedType -> ParsedType
pFunctionType params ret = ParsedType blankPos $ FunctionType params ret

pTypeCall :: ParsedType -> [ParsedType] -> ParsedType
pTypeCall typeFunc args = ParsedType blankPos $ TypeCall typeFunc args



blankPos :: SourcePos
blankPos = initialPos "test"


        
runTestCase :: (Show a, Eq a)
            => String -> EParser a -> String -> Maybe a -> IO ()
runTestCase name p text expected= do
  case runEParser "tests" text (p <* eof) of
    Left e -> case expected of
      Just x -> assertFailure $ unlines $
          [ name 
          , "expected: " ++ show x
          , "but parse failed with:\n   " ++ show e
          ]
      Nothing -> return ()
    Right x -> case expected of
      Just x' -> case x == x' of
        True -> return ()
        False -> assertEqual name x x'
      Nothing -> assertFailure $ unlines $
        [ name
        , "expected parse fail"
        , "but parse succeeded with:\n   " ++ show x
        ]




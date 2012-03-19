module ETests.Parser (parser_test) where

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.String
import Language.TheExperiment.AST
import Language.TheExperiment.Parser
-- import Language.TheExperiment.Parser.Types


testSource :: String
testSource = "test source"

data ParseTest a
  = ExpectSuccess String (Parser a) String a String
  | Failure String (Parser a) String

runTestParser :: Parser a -> String -> Either ParseError (a, String)
runTestParser p s = runParser g () testSource s
    where
        g = do
           x <- p
           State s' _ _ <- getParserState
           return (x, s')

parseSucceedsWith :: (TestComp a) => Parser a -> String -> a -> String -> (Bool, Maybe a)
parseSucceedsWith p s er pr =  case runTestParser p s of
    Right r | r `comp` (er, pr) -> (True, Just $ fst r)
            | otherwise         -> (False, Just $ fst r)
    _                           -> (False, Nothing)

parseFails :: Parser a -> String -> Bool
parseFails p s = case runTestParser p s of
    Left _  -> True
    Right _ -> False



parser_test :: [(Bool, String, String)]
parser_test =
  concat [ map check literal_tests
         , map check parse_tests
         -- , map check std_type_tests
         -- , map check type_tests
         ]

literal_tests :: [ParseTest Literal]
literal_tests = [ expect "\"Hello World\"" (StringLiteral "Hello World") ""
                , expect "\'C\'"           (CharLiteral 'C') ""
                , expect "0b110011"        (BinLiteral 51) ""
                , expect "0x12345678"      (HexLiteral 305419896) ""
                , expect "123.43435342"    (FloatLiteral "123.43435342" 123.43435342) ""
                ]
  where expect = ExpectSuccess "Literals" aLiteral

parse_tests :: [ParseTest ParsedType]
parse_tests =
    [ expect "Foo"       expectedName_Foo  ""
    , expect "(Foo)"     expectedName_Foo  ""
    , expect "a"         expectedTypeVar_a ""
    , expect "Bar a"     expectedTypeCall_Bar_a ""
    , expect "(Bar b)"   expectedTypeCall_ParenBar_b ""
    , expect "a, b -> c" expectedFunctionType ""
    , expect "Foo a, (a -> b) -> c" expectedCrazyFunctionType ""
    ]
  where
    expect = ExpectSuccess "ParsedType" aType
    pos = initialPos testSource

    expectedName_Foo       = (TypeName     { typeName     = "Foo", typePos = pos } )
    expectedTypeVar_a      = (TypeVariable { typeVariable = "a",   typePos = pos } )
    expectedTypeCall_Bar_a = (TypeCall     {
                                typeFunction = (TypeName { typeName = "Bar", typePos = pos }),
                                typeParams = [ (TypeVariable { typeVariable = "a", typePos = pos }) ],
                                typePos = pos})
    expectedTypeCall_ParenBar_b = (TypeCall     {
                                    typeFunction = (TypeName { typeName = "Bar", typePos = pos }),
                                    typeParams = [ (TypeVariable { typeVariable = "b", typePos = pos }) ],
                                    typePos = pos})
    expectedFunctionType = (FunctionType pos [TypeVariable { typeVariable = "a", typePos = pos},
                                              TypeVariable { typeVariable = "b", typePos = pos}]
                                             (TypeVariable { typeVariable = "c", typePos = pos}))
    expectedCrazyFunctionType =
      (FunctionType {
        typePos = pos,
        argTypes = [
          TypeCall {
              typePos = pos,
              typeFunction = TypeName {
                typePos = pos,
                typeName = "Foo"
              },
              typeParams = [ TypeVariable {typePos = pos, typeVariable = "a"} ]
            },
          FunctionType {
            typePos = pos,
            argTypes = [
              TypeVariable {typePos = pos, typeVariable = "a"}],
              returnType = TypeVariable {typePos = pos, typeVariable = "b"}
          }
        ],
        returnType = TypeVariable {typePos = pos, typeVariable = "c"}
      })

-- parse_tests :: [ParseTest IntType]
-- parse_tests = 
--   [ expect "Int8" Int8 ""
--   , expect "Int16" Int16 ""
--   , expect "Int32" Int32 ""
--   , expect "Int64" Int64 ""
--   , expect "Int8 " Int8 " "
--   , expect "UInt8" UInt8 ""
--   , expect "UInt16" UInt16 ""
--   , expect "UInt32" UInt32 ""
--   , expect "UInt64" UInt64 ""
--   , expect "UInt8 " UInt8 " "
--   , expect "Int8(" Int8 "("
--   , Failure "Int Types" aIntType "Int8a"
--   ]
--   where expect = ExpectSuccess "Int Types" aIntType
--   
-- std_type_tests :: [ParseTest StdType]
-- std_type_tests = 
--   [ expect "Void" Void ""
--   , expect "Char8" Char8 ""
--   , expect "Int32" (IntType Int32) ""
--   , expect "Bool" SBool ""
--   , expect "F32 " F32 " "
--   , expect "F64" F64 ""
--   , expect "Void(" Void "("
--   , Failure "Standard types" aStdType "Voids"
--   , Failure "Standard types" aStdType "Char88"
--   ]
--   where expect = ExpectSuccess "Std Types" aStdType
-- 
-- --type_tests :: (Show a, Eq a) => [ParseTest (Type a)]
-- type_tests :: [ParseTest (Type ())]
-- type_tests = 
--   [ expect "Void" (Std Void) ""
--   , expect "Purple" (TypeName "Purple") ""
--   ]
--   where expect = ExpectSuccess "Types" aType


check :: (TestComp a, Show a) => ParseTest a -> (Bool, String, String)
check (ExpectSuccess parseName aParser input result leftOver) = 
  let (wasSuccess, parsedResult) = parseSucceedsWith aParser input result leftOver
      message = (show input) ++ " expected to parse to <" ++ show result ++ "> but was actually <" ++ show parsedResult ++ ">"
  in (wasSuccess, parseName, message)
check (Failure parseName aParser input) =
  let wasFailure = parseFails aParser input
  in  ( wasFailure, parseName, input )


class (Eq a) => TestComp a where
  comp :: a -> a -> Bool
  comp a b = a == b

instance (TestComp a) => TestComp [a] where
 comp l1 l2 = and $ zipWith comp l1 l2

instance (TestComp a, TestComp b) => TestComp (a,b) where
  comp (a,b) (a', b') = (a `comp` a') && (b `comp` b')

instance TestComp Char
instance TestComp Literal

instance TestComp ParsedType where
  comp (TypeName { typeName = n  })
       (TypeName { typeName = n' }) = n `comp` n'
  comp (TypeVariable { typeVariable = v  })
       (TypeVariable { typeVariable = v' }) = v `comp` v'
  comp (TypeCall { typeFunction = f,  typeParams = p  })
       (TypeCall { typeFunction = f', typeParams = p' }) = (f `comp` f') && (p `comp` p')
  comp (FunctionType { argTypes = a, returnType = r })
       (FunctionType { argTypes = a', returnType = r' }) = (a `comp` a') && (r `comp` r')
  comp _ _ = False

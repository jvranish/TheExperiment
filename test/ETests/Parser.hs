module ETests.Parser (parser_test) where

import Text.Parsec
import Text.Parsec.String
import Language.TheExperiment.AST
import Language.TheExperiment.Parser
import Language.TheExperiment.Type


data ParseTest a
  = ExpectSuccess String (Parser a) String a String
  | Failure String (Parser a) String

runTestParser :: Parser a -> String -> Either ParseError (a, String)
runTestParser p s = runParser g () "HSpec tests" s
    where
        g = do
           x <- p
           State s' _ _ <- getParserState
           return (x, s')

parseSucceedsWith :: (Eq a) => Parser a -> String -> a -> String -> Bool
parseSucceedsWith p s er pr =  case runTestParser p s of
    Right r | r == (er, pr) -> True
    _                       -> False

parseFails :: Parser a -> String -> Bool
parseFails p s = case runTestParser p s of
    Left _ -> True
    Right _ -> False



parser_test :: [(Bool, String, String)]
parser_test =
  concat [
    map check literal_tests,
    map check parse_tests
  ]

literal_tests :: [ParseTest Literal]
literal_tests = [ expect "\"Hello World\"" (StringLiteral "Hello World") ""
                , expect "\'C\'"           (CharLiteral 'C') ""
                , expect "0b110011"        (BinLiteral 51) ""
                , expect "0x12345678"      (HexLiteral 305419896) ""
                , expect "123.43435342"    (FloatLiteral "123.43435342" 123.43435342) ""
                ]
  where expect = ExpectSuccess "Literals" aLiteral

parse_tests :: [ParseTest IntType]
parse_tests = 
  [ expect "Int8" Int8 ""
  , expect "Int16" Int16 ""
  , expect "Int32" Int32 ""
  , expect "Int64" Int64 ""
  , expect "Int8 " Int8 " "
  , expect "Int8(" Int8 "("
  , Failure "Int Types" aIntType "Int8a"
  ]
  where expect = ExpectSuccess "Int Types" aIntType

check :: (Eq a, Show a) => ParseTest a -> (Bool, String, String)
check (ExpectSuccess parseName aParser input result leftOver) = (parseSucceedsWith aParser input result leftOver, parseName, input ++ " expected, got " ++ show result)
check (Failure parseName aParser input) = (not $ parseFails aParser input, parseName, input)


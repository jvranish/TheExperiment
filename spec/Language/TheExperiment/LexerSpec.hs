module Language.TheExperiment.LexerSpec (main) where

import Text.Parsec hiding (space)
import Text.Parsec.String

import Test.Hspec

import Language.TheExperiment.Lexer

main :: IO Bool
main =  hspecB $ concat $ [ spaceSpecs
                          , lexemeSpecs
                          ]

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

spaceSpecs :: Specs
spaceSpecs = describe "space" [
    it "parses a space character out of the stream and returns it"
        (parseSucceedsWith space " sadf" ' ' "sadf"),

    it "fails when the next character in the stream is not a space"
        (parseFails space "sadf")
    ]

lexemeSpecs :: Specs
lexemeSpecs = describe "lexeme" [
    it "applies the passed in parser and returns the result" 
      (parseSucceedsWith (lexeme (char 'a')) "aasdf" 'a' "asdf")
    , 
    it "applies the passed in parser to the input stream and then parses \
       \zero or more spaces after, returns the value returned by the \
       \passed in parser"
      (parseSucceedsWith (lexeme (char 'a')) "a   b" 'a' "b")
    ]


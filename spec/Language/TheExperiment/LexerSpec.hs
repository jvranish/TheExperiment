module LexerSpec where

import Text.Parsec hiding (space)
import Text.Parsec.String

import Test.Hspec

import Language.TheExperiment.Lexer

runTestParser :: Parser a -> String -> Either ParseError a
runTestParser p s = runParser p () "HSpec tests" s

parseSucceedsWith :: (Eq a) => a -> Either ParseError a -> Bool
parseSucceedsWith x (Right y) = x == y
parseSucceedsWith _ _ = False

parseFails :: Either ParseError a -> Bool
parseFails (Left _) = True

spaceSpecs :: Specs
spaceSpecs = describe "space" [
    it "parses a space character out of the stream and returns it"
        (parseSucceedsWith ' ' $ runTestParser space " sadf"),

    it "fails when the next character in the stream is not a space"
        (parseFails $ runTestParser space "sadf")
    ]


lexemeSpecs :: Specs
lexemeSpecs = describe "lexeme" [
    it "returns the result of the passed in parser" (parseSucceedsWith 'a' $ runTestParser (lexeme (char 'a')) "aasdf")
    -- , 
    -- it "applies the passed in parser to the input stream and then parses zero or more spaces after, returns the value returned by the passed in parser"
    --    (parseSucceedsWith 'a' $ runTestParser (lexeme (char 'a')) "a   b")
    ]


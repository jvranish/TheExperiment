module Language.TheExperiment.Lexer where

import Text.Parsec hiding (space)
import Text.Parsec.String

import Test.Hspec


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

space :: Parser Char
space = char ' '


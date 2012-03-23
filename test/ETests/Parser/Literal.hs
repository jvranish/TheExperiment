module ETests.Parser.Literal
  ( testParseLiteral
  , aLiteralSpecs
  ) where

import Control.Applicative

import Text.Parsec
import Text.Parsec.Error

import Language.TheExperiment.AST.Expression
import Language.TheExperiment.Parser

import ETests.Utils

import Test.Hspec

testParseLiteral :: IO Specs
testParseLiteral = hspec aLiteralSpecs

aLiteralSpecs :: Specs
aLiteralSpecs = describe "aLiteral" $
    [ it "Parses a zero integer literal" $
        "0" `parsesTo` (Right $ IntegerLiteral 0)
    , it "Parses an integer literal" $
        "12345" `parsesTo` (Right $ IntegerLiteral 12345)
    , it "Parses an char literal" $
        "'a'" `parsesTo` (Right $ CharLiteral 'a')
    , it "Parses an string literal" $
        "\"asdf\"" `parsesTo` (Right $ StringLiteral "asdf")
    , it "Parses an binary literal" $
        "0b1010" `parsesTo` (Right $ BinLiteral 10)
    , it "Parses an binary literal with uppercase B" $
        "0B1010" `parsesTo` (Right $ BinLiteral 10)
    , it "Parses an hex literal" $
        "0x20AF" `parsesTo` (Right $ HexLiteral 8367)
    , it "Parses an hex literal with uppercase X" $
        "0X20" `parsesTo` (Right $ HexLiteral 32)
    , it "Parses an octal literal" $
        "0o2322" `parsesTo` (Right $ OctalLiteral 1234)
    , it "Parses an octal literal with uppercase O" $
        "0O2322" `parsesTo` (Right $ OctalLiteral 1234)
    , it "Parses an floating point literal" $
        "3.14159" `parsesTo` (Right $ FloatLiteral "3.14159" 3.14159)
    , it "Parses an floating point literal with leading zero" $
        "0.14159" `parsesTo` (Right $ FloatLiteral "0.14159" 0.14159)
    , it "Gives good error message for bad hex literal" $
        "0x" `parsesTo` (Left $ [SysUnExpect "", Expect "hexadecimal digit"])
    , it "Gives good error message for bad octal literal" $
        "0o" `parsesTo` (Left $ [SysUnExpect "", Expect "octal digit"])
    , it "Gives good error message for bad binary literal" $
        pending "This is broken, but not that important"
        --"0b" `parsesTo` (Left $ [SysUnExpect "", Expect "binary digit"])
    , it "Gives good error message for bad integer literal" $
        pending "make should this mentions potential floating point completions"
    , it "Gives good error messages" $
        pending "put some thought into this, there are several cases that are not perfect"
    , it "Parses a floating point number with exponent" $
        pending "add support for floating point with exponents"
    , it "Parses negative literals" $
        pending "I'm pretty sure we don't want to do this, the possible\
                \ exception are constant expressions, but we might handle\
                \ that in the code generator"
    ]
  where
    parsesTo input expected = eTestParse "aLiteral" expected (runEParser "tests" input (aLiteral <* eof))


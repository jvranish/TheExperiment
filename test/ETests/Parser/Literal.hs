module ETests.Parser.Literal
  ( testParseLiteral
  , aLiteralSpecs
  ) where


import Control.Applicative

import Text.Parsec

import Language.TheExperiment.AST.Expression
import Language.TheExperiment.Parser

import ETests.Utils

import Test.Hspec

testParseLiteral :: IO Specs
testParseLiteral = hspec aLiteralSpecs
-- #TODO there might be a problem with overlapping 
--  syntax, x20 might be confused with an identifier
-- 
aLiteralSpecs :: Specs
aLiteralSpecs = describe "aLiteral" $
    [ it "Parses an integer literal" $
        "12345" `parsesTo` (IntegerLiteral 12345)
    , it "Parses an negative integer literal" $
        "-12345" `parsesTo` (IntegerLiteral (-12345))
    , it "Parses an char literal" $
        "'a'" `parsesTo` (CharLiteral 'a')
    , it "Parses an string literal" $
        "\"asdf\"" `parsesTo` (StringLiteral "asdf")
    , it "Parses an binary literal" $
        "0b1010" `parsesTo` (BinLiteral 10)
    , it "Parses an binary literal with uppercase B" $
        "0B1010" `parsesTo` (BinLiteral 10)
    , it "Parses an hex literal" $
        "0x20AF" `parsesTo` (HexLiteral 8367)
    , it "Parses an hex literal with uppercase X" $
        "0X20" `parsesTo` (HexLiteral 32)
    , it "Parses an octal literal" $
        "0o2322" `parsesTo` (OctalLiteral 1234)
    , it "Parses an octal literal with uppercase O" $
        "0O2322" `parsesTo` (OctalLiteral 1234)
    , it "Parses an floating point literal" $
        "3.14159" `parsesTo` (FloatLiteral "3.14159" 3.14159)
    , it "Parses an floating point literal with leading zero" $
        "0.14159" `parsesTo` (FloatLiteral "0.14159" 0.14159)
    , it "Parses an floating point literal with negative sign" $
        "-0.14159" `parsesTo` (FloatLiteral "-0.14159" (-0.14159))
    
    ]
  where
    parsesTo input expected = case runEParser "tests" input (aLiteral <* eof) of
      Left e -> error $ "Fix your test: " ++ show e
      Right x -> eTestAssertEqual "aLiteral" expected x 

    
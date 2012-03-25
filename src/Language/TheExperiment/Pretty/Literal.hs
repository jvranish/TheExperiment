module Language.TheExperiment.Pretty.Literal
  ( prettyLiteral
  ) where

import Data.Char
import Numeric

import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.AST.Expression
import Language.TheExperiment.Parser.Literal


prettyLiteral :: Literal -> Doc
prettyLiteral (StringLiteral s) = doubleQuotes $ text s
prettyLiteral (CharLiteral c) = quotes $ char c
prettyLiteral (IntegerLiteral n) = integer n
prettyLiteral (BinLiteral n) = text "0b" <> text (showIntAtBase 2 intToDigit n "")
prettyLiteral (HexLiteral n) = text "0x" <> text (fmap toUpper $ showHex n "")
prettyLiteral (OctalLiteral n) = text "0o" <> text (showOct n "")
prettyLiteral (FloatLiteral s _) = text s
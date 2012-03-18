module Language.TheExperiment.Parser.Literal
  ( Literal(..)
  , aLiteral
  ) where

import Control.Monad

import Numeric
import Data.Maybe
import Text.Parsec

import Language.TheExperiment.Parser.Lexer

data Literal = StringLiteral String
             | CharLiteral Char
             | IntegerLiteral Integer
             | BinLiteral Integer
             | HexLiteral Integer
             | OctalLiteral Integer
             | FloatLiteral String Double -- String is parsed representation.
    deriving (Show, Eq, Ord)

readStdInt :: Num a => a -> String -> a
readStdInt base str = val
  where
    [(val,_)] = readInt base validDigit digitValue str
    digits = ['0'..'9'] ++ ['A'..'F']
    values = [0..]
    validDigit = (`elem` digits)
    digitValue x = fromJust $ lookup x (zip digits values)

aNumericLiteral :: EParser a -> [Char] -> (Integer -> Literal) -> EParser Literal
aNumericLiteral prefix digits cons = do
  _ <- prefix
  lit <- many1 $ oneOf digits
  return $ cons $ readStdInt (fromIntegral $ length digits) lit

aBinLiteral :: EParser Literal
aBinLiteral = aNumericLiteral prefix ['0'..'1'] BinLiteral
  where
    prefix = char '0' >> (char 'b' <|> char 'B')

aFloatLiteral :: EParser Literal
aFloatLiteral = do
  whole <- many1 $ oneOf ['0'..'9']
  d     <- char '.'
  frac  <- many1 $ oneOf ['0'..'9']

  let str = whole ++ (d : frac)
  return $ FloatLiteral str (read str)

aStringLiteral :: EParser Literal
aStringLiteral = liftM StringLiteral stringLiteral

aCharLiteral :: EParser Literal
aCharLiteral = liftM CharLiteral charLiteral

aHexLiteral :: EParser Literal
aHexLiteral = liftM HexLiteral hexadecimal

aOctalLiteral :: EParser Literal
aOctalLiteral = liftM OctalLiteral octal

aDecLiteral :: EParser Literal
aDecLiteral = liftM IntegerLiteral decimal

aLiteral :: EParser Literal
aLiteral = try aStringLiteral
       <|> try aCharLiteral
       <|> try aFloatLiteral
       <|> try aBinLiteral
       <|> try aHexLiteral
       <|> try aOctalLiteral
       <|>     aDecLiteral

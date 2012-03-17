module Language.TheExperiment.Parser.Literal where

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

aStringLiteral :: EParser Literal
aStringLiteral = do
  _ <- char '"'
  s <- many $ noneOf "\""
  _ <- char '"'

  return $ StringLiteral s

aCharLiteral :: EParser Literal
aCharLiteral = do
  _ <- char '\''
  c <- noneOf "\'"
  _ <- char '\''

  return $ CharLiteral c

aFloatLiteral :: EParser Literal
aFloatLiteral = do
  whole <- many1 $ oneOf ['0'..'9']
  d     <- char '.'
  frac  <- many1 $ oneOf ['0'..'9']

  let str = whole ++ (d : frac)
  return $ FloatLiteral str (read str)

aNumericLiteral :: Char -> [Char] -> (Integer -> Literal) -> EParser Literal
aNumericLiteral prefix digits cons = do
  _ <- char '0'
  _ <- char prefix
  lit <- many1 $ oneOf digits
  return $ cons $ readStdInt (fromIntegral $ length digits) lit

aBinLiteral :: EParser Literal
aBinLiteral = aNumericLiteral 'b' ['0'..'1'] BinLiteral

aHexLiteral :: EParser Literal
aHexLiteral = aNumericLiteral 'x' (['0'..'9'] ++ ['A'..'F']) HexLiteral

aOctalLiteral :: EParser Literal
aOctalLiteral = aNumericLiteral 'o' ['0'..'7'] OctalLiteral

aDecLiteral :: EParser Literal
aDecLiteral = do
  lit <- many1 $ oneOf ['0'..'9']
  return $ IntegerLiteral $ readStdInt 10 lit


aLiteral :: EParser Literal
aLiteral = try aStringLiteral
       <|> try aCharLiteral
       <|> try aFloatLiteral
       <|> try aBinLiteral
       <|> try aHexLiteral
       <|> try aOctalLiteral
       <|>     aDecLiteral

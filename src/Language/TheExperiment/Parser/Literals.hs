module Language.TheExperiment.Parser.Literals (aLiteral) where

import Numeric
import Data.Maybe
import Text.Parsec hiding (spaces)
import Text.Parsec.String

import Language.TheExperiment.AST

readStdInt :: Num a => a -> String -> a
readStdInt base str = val
  where
    [(val,_)] = readInt base validDigit digitValue str
    digits = ['0'..'9'] ++ ['A'..'F']
    values = [0..]
    validDigit = (`elem` digits)
    digitValue x = fromJust $ lookup x (zip digits values)

aStringLiteral :: Parser Literal
aStringLiteral = do
  _ <- char '"'
  s <- many $ noneOf "\""
  _ <- char '"'

  return $ StringLiteral s

aCharLiteral :: Parser Literal
aCharLiteral = do
  _ <- char '\''
  c <- noneOf "\'"
  _ <- char '\''

  return $ CharLiteral c

aFloatLiteral :: Parser Literal
aFloatLiteral = do
  whole <- many1 $ oneOf ['0'..'9']
  d     <- char '.'
  frac  <- many1 $ oneOf ['0'..'9']

  let str = whole ++ (d : frac)
  return $ FloatLiteral str (read str)

aNumericLiteral :: Char -> [Char] -> (Integer -> Literal) -> Parser Literal
aNumericLiteral prefix digits cons = do
  _ <- char '0'
  _ <- char prefix
  lit <- many1 $ oneOf digits
  return $ cons $ readStdInt (fromIntegral $ length digits) lit

aBinLiteral :: Parser Literal
aBinLiteral = aNumericLiteral 'b' ['0'..'1'] BinLiteral

aHexLiteral :: Parser Literal
aHexLiteral = aNumericLiteral 'x' (['0'..'9'] ++ ['A'..'F']) HexLiteral

aOctalLiteral :: Parser Literal
aOctalLiteral = aNumericLiteral 'o' ['0'..'7'] OctalLiteral

aDecLiteral :: Parser Literal
aDecLiteral = do
  lit <- many1 $ oneOf ['0'..'9']
  return $ IntegerLiteral $ readStdInt 10 lit


aLiteral :: Parser Literal
aLiteral = try aStringLiteral
       <|> try aCharLiteral
       <|> try aFloatLiteral
       <|> try aBinLiteral
       <|> try aHexLiteral
       <|> try aOctalLiteral
       <|>     aDecLiteral

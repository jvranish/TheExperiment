module Language.TheExperiment.Parser.Literal
  ( Literal(..)
  , aLiteral
  ) where

import Control.Monad

import Numeric
import Data.Maybe
import Text.Parsec

import Language.TheExperiment.AST.Expression
import Language.TheExperiment.Parser.Lexer

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
    prefix = try $ char '0' >> (char 'b' <|> char 'B')

aFloatLiteral :: EParser Literal
aFloatLiteral = do
  (prefix, whole, d) <- try $ do
    prefix <-  liftM return (char '-')
           <|> liftM return (char '+')
           <|> return ""
    whole <- many1 $ oneOf ['0'..'9']
    d     <- char '.'
    return (prefix, whole, d)

  frac  <- many1 $ oneOf ['0'..'9']

  let str = prefix ++ whole ++ (d : frac)
  return $ FloatLiteral str (read str)

sign :: (Num a) => EParser (a -> a)
sign = (char '-' >> return negate)
   <|> (char '+' >> return id)
   <|> return id

aStringLiteral :: EParser Literal
aStringLiteral = liftM StringLiteral stringLiteral

aCharLiteral :: EParser Literal
aCharLiteral = liftM CharLiteral charLiteral

aHexLiteral :: EParser Literal
aHexLiteral = liftM HexLiteral ((try $ char '0' >> oneOf "xX") >> number 16 hexDigit)

aOctalLiteral :: EParser Literal
aOctalLiteral = liftM OctalLiteral ((try $ char '0' >> oneOf "oO") >> number 8 octDigit)

aDecLiteral :: EParser Literal
aDecLiteral = liftM IntegerLiteral $ do
  f <- sign
  liftM f decimal

aLiteral :: EParser Literal
aLiteral = aStringLiteral
       <|> aCharLiteral
       <|> aFloatLiteral
       <|> aBinLiteral
       <|> aHexLiteral
       <|> aOctalLiteral
       <|> aDecLiteral

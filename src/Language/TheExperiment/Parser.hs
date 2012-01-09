module Language.TheExperiment.Parser where

{- General structure of types:
 - 
 - Module
 - \
 -  TopLevelStmt
 -   \
 -    Statement
 -    \
 -     Expr
 - 
 - The type parameter to Module, TopLevelStmt, Statement, and Expr is () for
 - the parser stage. This information is filled in later by the inference
 - engine and the type checker.
 -
 -}

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Maybe
import Numeric

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
       <|> try aBinLiteral
       <|> try aHexLiteral
       <|> try aOctalLiteral
       <|>     aDecLiteral

parseExpr :: String -> String
parseExpr input = case parse aLiteral "lisp" input of
                    Left err -> "No match: " ++ show err
                    Right val -> show val


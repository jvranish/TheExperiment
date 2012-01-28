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

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Data.Maybe
import Numeric

import Language.TheExperiment.AST
import Language.TheExperiment.Type

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

aTypeName :: Parser ParsedType
aTypeName = undefined

keyword :: String -> Parser ()
keyword s = try $ do
  string s
  notFollowedBy alphaNum

aIntType :: Parser IntType
aIntType = (try aInt8 )
       <|> (try aInt16)
       <|> (try aInt32)
       <|> (try aInt64)
       <|> (try aUInt8 )
       <|> (try aUInt16)
       <|> (try aUInt32)
       <|> (    aUInt64)
  where
    aInt8   = keyword "Int8" >> return Int8
    aInt16  = keyword "Int16" >> return Int16
    aInt32  = keyword "Int32" >> return Int32
    aInt64  = keyword "Int64" >> return Int64
    aUInt8  = keyword "UInt8" >> return UInt8
    aUInt16 = keyword "UInt16" >> return UInt16
    aUInt32 = keyword "UInt32" >> return UInt32
    aUInt64 = keyword "UInt64" >> return UInt64



parseExpr :: String -> Literal
parseExpr input = case parse aLiteral "TheExperiment" input of
                    Left err   -> error $ "Failed to parse the string \"" ++ input ++ "\": " ++ show err
                    Right good -> good


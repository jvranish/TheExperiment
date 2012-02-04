module Language.TheExperiment.Parser
  ( module Language.TheExperiment.Parser.Literals
  , aStdType
  , aType
  , aIntType
) where

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
import Control.Monad

import Language.TheExperiment.Type
import Language.TheExperiment.Parser.Literals

keyword :: String -> Parser ()
keyword s = try $ do
  _ <- string s
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

aStdType :: Parser StdType
aStdType = (try aVoid)
       <|> (try aChar8)
       <|> (try aStdIntType)
       <|> (try aBool)
       <|> (try aF32)
       <|> (try aF64)
  where
    aVoid = keyword "Void" >> return Void
    aChar8 = keyword "Char8" >> return Char8
    aStdIntType = liftM IntType aIntType
    aBool = keyword "Bool" >> return SBool
    aF32 = keyword "F32" >> return F32
    aF64 = keyword "F64" >> return F64

aType :: Parser (Type a)
aType = (try aTypeStd)
    <|> (try aTypeName)
  where 
    aTypeStd = liftM Std aStdType
    aTypeName = do
      firstLetter <- oneOf ['A'..'Z']
      rest <- many alphaNum
      return $ TypeName $ firstLetter : rest


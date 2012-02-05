module Language.TheExperiment.Parser.Types
  ( aType,
) where

import Text.Parsec hiding (spaces)
import Text.Parsec.String

import Language.TheExperiment.AST (ParsedType(..))

-- data Type = CompositeType String [Type]
--           | KindedType KindedType -- (Foo a) => a Int
--           | TypeVariable TypeVariable
--           | NumericType Int
--           | StructuredType String [(String, Type)]
--           | Function [Type] Type
--   deriving (Show)

-- data KindedType = KindedType [TypeVariable] Type
--   deriving (Show)
-- 
-- data TypeVariable = TypeVariable String
--   deriving (Show)

-- foo :: (Int, a || {zap :: Int, boop :: Int}, Bar) -> c
-- foo(num, a, thing):
--   case thing of
--     1 -> num + a.zap
--     2 -> num + a.boop
-- 
-- 
-- foo (b , a | {zap :: Int, boop :: b}) => (Int, a, Bar) -> c
-- 
-- b | {Int | Float | zap :: Int, boop :: b}
-- 
-- foo ||
--   (a, b, c) -> d |
--     a :: Int
--     b :: {x, y}
--     c

-- HAMMER OF CONFORMITY!!!!!
aTypeName :: Parser ParsedType
aTypeName = do
  pos <- getPosition
  firstLetter <- oneOf ['A'..'Z']
  rest <- many alphaNum
  return $ TypeName { typePos = pos, typeName = firstLetter : rest }

aTypeVariable :: Parser ParsedType
aTypeVariable = do
  pos <- getPosition
  firstLetter <- oneOf ['a'..'z']
  rest <- many alphaNum
  return $ TypeVariable { typePos = pos, typeVariable = firstLetter : rest }

aType :: Parser ParsedType
aType = aTypeName <|> aTypeVariable

module Language.TheExperiment.Parser.Types
  ( aType,
) where

import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Text.Parsec.Char

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

symbol :: String -> Parser String
symbol = lexeme . string

lexeme :: Parser a -> Parser a
lexeme p = do
  a <- p
  spaces
  return a
 

paren :: Parser a -> Parser a
paren p = between (lexeme $ char '(') (lexeme $ char ')') p

aTypeTerm :: Parser ParsedType
aTypeTerm = aTypeName <|> aTypeVariable <|> paren aType <?> "Type Term"


aType :: Parser ParsedType
aType = do
  pos        <- getPosition
  paramTypes <- sepBy1 aTypeCall (char ',')
  funcReturn <- optionMaybe $ do
      _ <- symbol "->"
      aTypeTerm
  case (paramTypes, funcReturn) of
    (ts , Just ret) -> return $ FunctionType pos ts ret
    ([t], Nothing)  -> return $ t
    (_  , Nothing)  -> parserZero <?> "->"


aTypeCall :: Parser ParsedType
aTypeCall = do
    pos         <- getPosition
    typeFunc    <- aTypeTerm
    typeParams' <- many aTypeTerm
    return $ case typeParams' of
               [] -> typeFunc
               _  -> TypeCall { typePos      = pos
                              , typeFunction = typeFunc
                              , typeParams   = typeParams'
                              }

aTypeName :: Parser ParsedType
aTypeName = lexeme $ do
  pos <- getPosition
  firstLetter <- oneOf ['A'..'Z'] -- HAMMER OF CONFORMITY!!!!!
  rest <- many alphaNum
  return $ TypeName { typePos = pos, typeName = firstLetter : rest }

aTypeVariable :: Parser ParsedType
aTypeVariable = lexeme $ do
  pos <- getPosition
  firstLetter <- oneOf ['a'..'z']
  rest <- many alphaNum
  return $ TypeVariable { typePos = pos, typeVariable = firstLetter : rest }

-- aTypeFunction :: Parser ParsedType
-- (a, b) -> c
--
-- Foo (a, b -> c) Int
-- foo :: a, b -> c
--
-- Foo (a, b -> c) Int

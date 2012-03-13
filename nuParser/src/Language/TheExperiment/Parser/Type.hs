module Language.TheExperiment.Parser.Type where

import Text.Parsec
import Text.Parsec.String
-- import qualified Text.Parsec.Token as T

import Language.TheExperiment.Parser.Lexer

data ParsedType = TypeName      { typePos :: SourcePos
                                , typeName :: String
                                } | -- Int, Var, Foo, Void 
                  TypeVariable  { typePos :: SourcePos
                                , typeVariable :: String
                                } | -- a, b, c, d, bees
                  TypeCall      { typePos :: SourcePos
                                , typeFunction :: ParsedType
                                , typeParams :: [ParsedType]
                                } | -- Foo a Int, Foo a b, Foo (Foo Var)
                  FunctionType  { typePos :: SourcePos
                                , argTypes :: [ParsedType]
                                , returnType :: ParsedType
                                }
  deriving (Show, Eq, Ord)

aTypeName :: Parser ParsedType
aTypeName = lexeme $ liftMp TypeName typeIdent

aTypeVariable :: Parser ParsedType
aTypeVariable = lexeme $ liftMp TypeVariable varIdent

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

aTypeTerm :: Parser ParsedType
aTypeTerm = aTypeName <|> aTypeVariable <|> parens aParsedType <?> "Type Term"

aParsedType :: Parser ParsedType
aParsedType = do
  pos        <- getPosition
  paramTypes <- sepBy1 aTypeCall comma
  funcReturn <- optionMaybe $ do
      _ <- symbol "->"
      aTypeTerm
  case (paramTypes, funcReturn) of
    (ts , Just ret) -> return $ FunctionType pos ts ret
    ([t], Nothing)  -> return $ t
    (_  , Nothing)  -> parserZero <?> "->"

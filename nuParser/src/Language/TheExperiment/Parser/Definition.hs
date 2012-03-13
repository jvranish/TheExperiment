module Language.TheExperiment.Parser.Definition where

import Text.Parsec
import Text.Parsec.String

import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Type
import Language.TheExperiment.Parser.Statement

data Definition = TypeDef { defnPos     :: SourcePos
                          , typeDefName :: String
                          , typeDefType :: ParsedType
                          }
                | TypeSignature { defnPos      :: SourcePos
                                , typeSigNames :: [String]
                                , typeSigType  :: ParsedType
                                }
                | VariableDef { defnPos         :: SourcePos
                              , variableDefName :: String
                              -- , initExpr :: Maybe (Expr a)
                              }
                | ForeignDef { defnPos       :: SourcePos
                             , nativeDefName :: String
                             , foreignName   :: String
                             , nativeDefType :: ParsedType
                             } -- foreign cFunction "c_function" (Int -> Int)
                | FunctionDef { defnPos           :: SourcePos
                              , functionName      :: String
                              , functionArgs      :: [String]
                              , functionStatement :: Statement
                              }
  deriving (Show, Eq, Ord)

aDefinition :: Parser Definition
aDefinition = try aTypeSignature -- must come before function def
          <|> try aFunctionDef
          <|> aTypeDef
          <|> aVariableDef
          <|> aForeignDef

{--}

aTypeDef :: Parser Definition
aTypeDef = do
  p <- getPosition
  _ <- reserved "type"
  v <- typeIdent
  _ <- reservedOp "="
  t <- aParsedType
  return $ TypeDef p v t

aTypeSignature :: Parser Definition
aTypeSignature = do
  p  <- getPosition
  vs <- commaSep1 varIdent
  _  <- reservedOp "::"
  t  <- aParsedType
  return $ TypeSignature p vs t

aVariableDef :: Parser Definition
aVariableDef = do
  p <- getPosition
  _ <- reserved "var"
  v <- varIdent
  return $ VariableDef p v

aForeignDef :: Parser Definition
aForeignDef = do
  p <- getPosition
  _ <- reserved "foreign"
  l <- varIdent
  f <- stringLiteral
  t <- aParsedType
  return $ ForeignDef p l f t

aFunctionDef :: Parser Definition
aFunctionDef = do
  p  <- getPosition
  n  <- varIdent
  as <- option [] $ parens $ commaSep1 varIdent
  _  <- reservedOp ":"
  return $ FunctionDef p n as Statement

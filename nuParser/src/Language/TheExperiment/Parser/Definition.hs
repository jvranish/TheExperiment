module Language.TheExperiment.Parser.Definition where

import Text.Parsec
import Text.Parsec.Indent

import Language.TheExperiment.Parser.Type
import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Expression

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
                              -- , initializationExpr :: Maybe (Expr a)
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

data Statement = Block  { stmtPos    :: SourcePos
                        , blockBody  :: [Either Definition Statement]
                        }
               | Return { stmtPos    :: SourcePos
                        , returnExpr :: Expr
                        }
  deriving (Show, Eq, Ord)

aDefinition :: EParser Definition
aDefinition = aTypeDef
          <|> aTypeSignature
          <|> aVariableDef
          <|> aForeignDef
          <|> aFunctionDef

{--}

aTypeDef :: EParser Definition
aTypeDef = do
  p <- getPosition
  _ <- reserved "type"
  v <- typeIdent
  _ <- reservedOp "="
  t <- aParsedType
  return $ TypeDef p v t

aTypeSignature :: EParser Definition
aTypeSignature = do
  p  <- getPosition
  vs <- commaSep1 varIdent
  _  <- reservedOp "::"
  t  <- aParsedType
  return $ TypeSignature p vs t

aVariableDef :: EParser Definition
aVariableDef = do
  p <- getPosition
  _ <- reserved "var"
  v <- varIdent
  return $ VariableDef p v

aForeignDef :: EParser Definition
aForeignDef = do
  p <- getPosition
  _ <- reserved "foreign"
  l <- varIdent
  f <- stringLiteral
  t <- aParsedType
  return $ ForeignDef p l f t

aFunctionDef :: EParser Definition
aFunctionDef = do
  p  <- getPosition
  _  <- reserved "def"
  n  <- varIdent
  as <- option [] $ parens $ commaSep1 varIdent
  _  <- reservedOp ":"
  s  <- aStatement
  return $ FunctionDef p n as s

aStatement :: EParser Statement
aStatement = do
  aReturn <|> aBlock

aBlock :: EParser Statement
aBlock = do
  p <- getPosition
  b <- block defOrStmt
  return $ Block p b
  where
    defOrStmt = do
      d <- optionMaybe aDefinition
      case d of
        Just d' -> return $ Left d'
        Nothing -> do
          s <- optionMaybe aStatement
          case s of
            Just s' -> return $ Right s'
            Nothing -> fail "Expected a Definition or a Statement, found enither."

aReturn :: EParser Statement
aReturn = do
  p <- getPosition
  _ <- reserved "return"
  e <- anExpr
  return $ Return p e

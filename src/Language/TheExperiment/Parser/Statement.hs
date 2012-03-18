module Language.TheExperiment.Parser.Definition where

import Control.Monad

import Text.Parsec
import Text.Parsec.Indent

import Language.TheExperiment.AST.Statement
-- import Language.TheExperiment.Parser.Type
import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Expression

type ParsedDefinition = Definition ()
type ParsedStatement = Statement ()

aParsedType = undefined

aDefinition :: EParser ParsedDefinition
aDefinition = aTypeDef
          <|> aTypeSignature
          <|> aVariableDef
          <|> aForeignDef
          <|> aFunctionDef

aTypeDef :: EParser ParsedDefinition
aTypeDef = do
  p <- getPosition
  _ <- reserved "type"
  v <- typeIdent
  _ <- reservedOp "="
  t <- aParsedType
  return $ TypeDef p v t

aTypeSignature :: EParser ParsedDefinition
aTypeSignature = do
  p  <- getPosition
  vs <- commaSep1 varIdent
  _  <- reservedOp "::"
  t  <- aParsedType
  return $ TypeSignature p vs t

aVariableDef :: EParser ParsedDefinition
aVariableDef = do
  p <- getPosition
  _ <- reserved "var"
  v <- varIdent
  return $ VariableDef p v

aForeignDef :: EParser ParsedDefinition
aForeignDef = do
  p <- getPosition
  _ <- reserved "foreign"
  l <- varIdent
  f <- stringLiteral
  t <- aParsedType
  return $ ForeignDef p l f t

aFunctionDef :: EParser ParsedDefinition
aFunctionDef = do
  p  <- getPosition
  _  <- reserved "def"
  n  <- varIdent
  as <- option [] $ parens $ commaSep1 varIdent
  _  <- reservedOp ":"
  s  <- aStatement
  return $ FunctionDef p n as s

        -- = Assign   { stmtPos      :: SourcePos
        --            , stmtNodeData :: a
        --            , assignName   :: String
        --            , assignExpr   :: Expr a
        --            }
        -- | If       { stmtPos      :: SourcePos
        --            , stmtNodeData :: a
        --            , ifCond       :: Expr a
        --            , ifThen       :: Statement a
        --            , ifElse       :: Maybe (Statement a)
        --            }
        -- | While    { stmtPos      :: SourcePos
        --            , stmtNodeData :: a
        --            , whileCond    :: Expr a
        --            , whileBody    :: Statement a
        --            }
        -- | ExprStmt { stmtPos      :: SourcePos
        --            , stmtNodeData :: a
        --            , stmtExpr     :: Expr a
        --            }
        -- | Return   { stmtPos      :: SourcePos
        --            , stmtNodeData :: a
        --            , returnExpr   :: Expr a
        --            }
        -- | Block    { stmtPos      :: SourcePos
        --            , stmtNodeData :: a
        --            , blockBody    :: ([ParsedDefinition a], [Statement a])
        --            }
  


aStatement :: EParser ParsedStatement
aStatement = do
  aReturn <|> aBlock

aBlock :: EParser ParsedStatement
aBlock = liftMp Block (block $ liftM2 (,) (many aDefinition) (many aStatement))

aReturn :: EParser ParsedStatement
aReturn = liftMp Return (reserved "return" >> anExpr)

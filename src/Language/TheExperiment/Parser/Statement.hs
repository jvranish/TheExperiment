module Language.TheExperiment.Parser.Statement where

import Control.Applicative hiding ((<|>))
import Control.Monad

import Text.Parsec
import Text.Parsec.Indent

import Language.TheExperiment.AST.Statement
import Language.TheExperiment.Parser.Type
import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Expression

type ParsedDefinition     = Definition ()
type ParsedStatement      = Statement ()
type ParsedVariable       = Variable ()
type ParsedDefOrStatement = DefOrStatement ()

aDefinition :: EParser ParsedDefinition
aDefinition = aTypeDef
          <|> aDefSignature
          <|> aVariableDef
          <|> aForeignDef
          <|> aFunctionDef

aTypeDef :: EParser ParsedDefinition
aTypeDef = liftM2p TypeDef (reserved "type" >> upperIdent) (reservedOp "=" >> aType)

aDefSignature :: EParser ParsedDefinition
-- aDefSignature = liftM2p DefSignature (commaSep1 lowerIdent) (reservedOp "::" >> aTypeSignature)
aDefSignature = liftM2p DefSignature (try $ commaSep1 lowerIdent <* reservedOp "::") aTypeSignature

aVariableDef :: EParser ParsedDefinition
aVariableDef = liftMp VariableDef (reserved "var" >> aVariable)

aForeignDef :: EParser ParsedDefinition
aForeignDef = liftM2p ForeignDef (reserved "foreign" >> lowerIdent) stringLiteral
 
aFunctionDef :: EParser ParsedDefinition
aFunctionDef = liftM4p FunctionDef (reserved "def" >> lowerIdent)
                                   (option [] $ parens $ commaSep aVariable)
                                   (return ())
                                   (reservedOp ":" >> aRawBlock)

aVariable :: EParser ParsedVariable
aVariable = liftMp Variable lowerIdent

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
aStatement = aReturn
         <|> aAssign
         <|> aCallStmt
         <|> aBlock
         <|> anIf
         <|> aWhile

aReturn :: EParser ParsedStatement
aReturn = liftMp Return (reserved "return" >> anExpr)

aAssign :: EParser ParsedStatement
aAssign = liftM2p Assign (try $ identifier <* reservedOp "=") anExpr

aCallStmt :: EParser ParsedStatement
aCallStmt = liftMp CallStmt aCall

aRawBlock :: EParser ParsedStatement
aRawBlock = liftMp Block $ block aDefOrStatement

aBlock :: EParser ParsedStatement
aBlock = liftMp Block (reserved "block" >> reservedOp ":" >> block aDefOrStatement)

anIf :: EParser ParsedStatement
anIf = do
  p <- getPosition
  reserved "if"
  e <- anExpr
  reservedOp ":"
  thenBlock <- aRawBlock
  ifElseOrElif <- optionMaybe anElseOrElif
  return $ If p () e thenBlock ifElseOrElif
  where
    anElseOrElif = anElse <|> anElif
    anElse = do
      reserved "else"
      reservedOp ":"
      aRawBlock
    anElif = do
      pos <- getPosition
      reserved "elif"
      e <- anExpr
      reservedOp ":"
      elifBlock <- aRawBlock
      elifNext <- optionMaybe anElseOrElif
      return $ If pos () e elifBlock elifNext

aWhile :: EParser ParsedStatement
aWhile = do
  p <- getPosition
  reserved "while"
  e <- anExpr
  reservedOp ":"
  whileBlock <- aRawBlock
  return $ While p () e whileBlock

aDefOrStatement :: EParser ParsedDefOrStatement
aDefOrStatement = liftM Def  aDefinition
              <|> liftM Stmt aStatement


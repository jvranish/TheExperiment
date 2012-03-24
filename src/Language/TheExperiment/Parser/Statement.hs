module Language.TheExperiment.Parser.Statement where

import Control.Monad

import Text.Parsec
import Text.Parsec.Indent

import Language.TheExperiment.AST.Statement
import Language.TheExperiment.AST.Type
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
aDefSignature = liftM2p DefSignature (commaSep1 lowerIdent) (reservedOp "::" >> aTypeSignature)

aVariableDef :: EParser ParsedDefinition
aVariableDef = liftMp VariableDef (reserved "var" >> aVariable)

aForeignDef :: EParser ParsedDefinition
aForeignDef = liftM2p ForeignDef (reserved "foreign" >> lowerIdent) stringLiteral
 
aFunctionDef :: EParser ParsedDefinition
aFunctionDef = liftM4p FunctionDef (reserved "def" >> lowerIdent)
                                   (option [] $ parens $ commaSep1 aVariable)
                                   (return ())
                                   (reservedOp ":" >> aStatement)

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
         <|> aIf
         <|> aWhile
         <|> aCallStmt
         <|> aBlock


aAssign :: EParser ParsedStatement
aAssign = liftM2p Assign identifier ((reservedOp "=") >> anExpr)

aIf :: EParser ParsedStatement
aIf = undefined

aWhile :: EParser ParsedStatement
aWhile = undefined

aCallStmt :: EParser ParsedStatement
aCallStmt = undefined

aReturn :: EParser ParsedStatement
aReturn = liftMp Return (reserved "return" >> anExpr)


aBlock :: EParser ParsedStatement
aBlock = liftMp Block (block $ aDefOrStatement)

aDefOrStatement :: EParser ParsedDefOrStatement
aDefOrStatement = liftM Def aDefinition
              <|> liftM Stmt aStatement








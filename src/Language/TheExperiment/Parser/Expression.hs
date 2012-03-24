module Language.TheExperiment.Parser.Expression where

import Text.Parsec

import Language.TheExperiment.AST.Expression
import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Literal


-- chain error things together
type ParsedExpr = Expr ()

anExpr :: EParser ParsedExpr
anExpr = liftM2p Identifier lowerIdent (return NotOperator) 
     <|> liftMp Literal aLiteral 
     <?> "expression"


-- undefined -- liftMp Literal aLiteral

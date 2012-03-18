module Language.TheExperiment.Parser.Expression where

import Text.Parsec

import Language.TheExperiment.AST.Expression
import Language.TheExperiment.Parser.Lexer
--import Language.TheExperiment.Parser.Literal



type ParsedExpr = Expr ()

anExpr :: EParser ParsedExpr
anExpr = undefined -- liftMp Literal aLiteral

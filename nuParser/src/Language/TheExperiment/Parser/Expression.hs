module Language.TheExperiment.Parser.Expression where

import Text.Parsec

import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Literal

data Expr = ExprLiteral {
		exprPos :: SourcePos,
		literal :: Literal
	}
	deriving (Show, Ord, Eq)

anExpr :: EParser Expr
anExpr = liftMp ExprLiteral aLiteral

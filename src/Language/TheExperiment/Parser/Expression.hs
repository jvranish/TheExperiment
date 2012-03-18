module Language.TheExperiment.Parser.Expression where

import Text.Parsec

import Language.TheExperiment.Parser.Lexer
--import Language.TheExperiment.Parser.Literal

data Expr = Literal {
    exprPos :: SourcePos
    -- literal :: Literal
  }
  -- Identifier
  -- Call
  deriving (Show, Ord, Eq)

anExpr :: EParser Expr
anExpr = undefined -- liftMp Literal aLiteral

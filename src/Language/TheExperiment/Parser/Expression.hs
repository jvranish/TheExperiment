module Language.TheExperiment.Parser.Expression where

import Text.Parsec
import Text.Parsec.Expr

import Data.List
import Data.Function

import Language.TheExperiment.Parser.AST.Expression
import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Literal

type ParsedExpr = Expr ()


anExpr :: EParser ParsedExpr
anExpr = do
  Operators opList <- getState
  let opTable = fmap (fmap fst) $ groupBy ((==) `on` snd) $ reverse $ sortBy (compare `on` snd) opList
  --let opTable = [ op | (op, prec) <- opList, then reverse ... sortWith by prec, then group by prec]
  buildExpressionParser opTable aSmplExpr <?> "expression"


aSmplExpr :: EParser ParsedExpr
aSmplExpr = anIdOrCall
        <|> parens anExpr
        <|> liftMp Literal aLiteral 
        <?> "simple expression"

aCall :: EParser ParsedExpr
aCall = do
  expr <- anIdOrCall
  case expr of
    Call {} -> return expr
    _       -> parserZero <?> "function call"

anIdOrCall :: EParser ParsedExpr
anIdOrCall = do
  pos <- getPosition
  name <- lowerIdent
  args <- optionMaybe $ parens $ sepBy anExpr comma
  let ident = Identifier pos () name NotOperator
  return $ case args of
    Nothing -> ident
    Just xs -> Call pos () ident xs

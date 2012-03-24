module Language.TheExperiment.Parser.Expression where

import Control.Monad

import Text.Parsec
import Text.Parsec.Expr

import Data.List
import Data.Function

import Language.TheExperiment.AST.Expression
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


{-liftM2p Identifier lowerIdent (return NotOperator) 
The syntax goes like this:
infixl + add 5

The function is a bit of a mind bender due to the wierdness
 of the Parsec Operator type. I've written an equivalent to this 
 function at least five times and it still confuses me.
-}
parseOpDef :: EParser (ParserOperator, Rational)
parseOpDef = parseOpType "infixr"  InR  (flip Infix AssocRight . liftM call2)
         <|> parseOpType "infix"   In   (flip Infix AssocNone . liftM call2)
         <|> parseOpType "infixl"  InL  (flip Infix AssocLeft . liftM call2)
         <|> parseOpType "prefix"  Pre  (Prefix . liftM call)
         <|> parseOpType "postfix" Post (Postfix . liftM call)
         <?> "operator definition"
  where
    parseOpType :: String                                -- tag
                -> (Rational -> OpFormat)                -- fixityCons
                -> (EParser (Expr ()) -> ParserOperator)  -- opCons
                -- take a parser for an operator and returns a
                -- ParserOperator suitable for an expression parser
                -- operator table
                -> EParser (ParserOperator, Rational)
    parseOpType tag fixityCons opCons = do
      --opCons :: EParser () -> ParserOperator
      reserved tag
      opName <- operator
      name <- identifier
      precedence <- rational
      let opParser = do
          pos <- getPosition
          reservedOp opName
          return $ Identifier { exprPos = pos
                              , exprNodeData = ()
                              -- #TODO find a more general solution here:
                              , idName = "Builtin." ++ name
                              , opFormat = fixityCons precedence }
      return (opCons opParser, precedence)
    call f a    = call' f [a]
    call2 f a b = call' f [a, b]
    call' f xs  = Call { exprPos = exprPos f
                       , exprNodeData = ()
                       , callFunc = f
                       , callParams = xs
                       }

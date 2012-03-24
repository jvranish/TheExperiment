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

{-
{-
The syntax goes like this:
infixl + add 5

The function is a bit of a mind bender due to the wierdness
 of the Parsec Operator type. I've written an equivalent to this 
 function at least five times and it still confuses me.
-}
parseOpDef :: Parser (ParserOperator, Rational)
parseOpDef = parseOpType "infixr"  InR  (flip Infix AssocRight . liftM call2)
         <|> parseOpType "infix"   In   (flip Infix AssocNone . liftM call2)
         <|> parseOpType "infixl"  InL  (flip Infix AssocLeft . liftM call2)
         <|> parseOpType "prefix"  Pre  (Prefix . liftM call)
         <|> parseOpType "postfix" Post (Postfix . liftM call)
         <?> "operator definition"
  where
    parseOpType :: String                                -- tag
                -> (Rational -> OpFormat)                -- fixityCons
                -> (Parser (Expr ()) -> ParserOperator)  -- opCons
                -- take a parser for an operator and returns a
                -- ParserOperator suitable for an expression parser
                -- operator table
                -> Parser (ParserOperator, Rational)
    parseOpType tag fixityCons opCons = do
      --opCons :: Parser () -> ParserOperator
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
-}
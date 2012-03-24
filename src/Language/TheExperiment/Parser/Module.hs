
module Language.TheExperiment.Parser.Module where

import Control.Monad

import Text.Parsec
import Text.Parsec.Expr

import Language.TheExperiment.AST.Module
import Language.TheExperiment.AST.Expression
import Language.TheExperiment.Parser.Lexer
import Language.TheExperiment.Parser.Statement


type ParsedModule = Module ()

parseSource :: EParser ParsedModule
parseSource = parseLex $ many anOpDef >>= putState . Operators >> aModule

parseLex :: EParser a -> EParser a
parseLex p = do 
  whiteSpace
  x <- p
  eof
  return x

aModule :: EParser ParsedModule
aModule = do
  pos <- getPosition -- hmm do I really want this?
  defs <- many aDefinition
  return $ Module pos defs

{-
The syntax goes like this:
infixl + add 5

The function is a bit of a mind bender due to the wierdness
 of the Parsec Operator type. I've written an equivalent to this 
 function at least five times and it still confuses me.
-}
anOpDef :: EParser (ParserOperator, Rational)
anOpDef = anOpType "infixr"  InR  (flip Infix AssocRight . liftM call2)
      <|> anOpType "infix"   In   (flip Infix AssocNone . liftM call2)
      <|> anOpType "infixl"  InL  (flip Infix AssocLeft . liftM call2)
      <|> anOpType "prefix"  Pre  (Prefix . liftM call)
      <|> anOpType "postfix" Post (Postfix . liftM call)
      <?> "operator definition"
  where
    anOpType :: String                                -- tag
             -> (Rational -> OpFormat)                -- fixityCons
             -> (EParser (Expr ()) -> ParserOperator)  -- opCons
             -- take a parser for an operator and returns a
             -- ParserOperator suitable for an expression parser
             -- operator table
             -> EParser (ParserOperator, Rational)
    anOpType tag fixityCons opCons = do
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


module Language.TheExperiment.TempParser where

import Text.Parsec
import Text.Parsec.Expr

import Control.Monad

import Numeric
import Data.Maybe
import Data.List
import Data.Function
import Data.Functor.Identity

import Language.TheExperiment.TempLexer
import Language.TheExperiment.AST

import Language.TheExperiment.Parser.Types

-- #TODO change naming convention to the aThing rather than parseThing

-- #TODO add multi module parsing
-- #TODO prefix identifiers with module name
-- #TODO module importing?

parseFile :: String -> IO (Either ParseError (Module ()))
parseFile filename  =  do 
  input <- readFile filename
  -- the seq is here because of the stupid lazy IO! >:|
  return $ seq (length input) $ runIdentity $ runParserT parseSource (Operators []) filename input

parseSource :: Parser (Module ())
parseSource = parseLex $ many parseOpDef >>= putState . Operators >> parseModule

parseLex :: Parser a -> Parser a
parseLex p = do 
  whiteSpace
  x <- p
  eof
  return x

parseModule :: Parser (Module ())
parseModule = do
  pos <- getPosition -- hmm do I really want this?
  topStmts <- many parseDefinition
  return $ Module pos topStmts



parseDefinition :: Parser (Definition ())
-- #TODO can we get rid of this try?
parseDefinition = try parseVarDef
                <|> try parseFuncDef
                <|> parseForeign
                -- <|> parseTypeDef
                <?> "definition"

parseSignature :: Parser TypeSig
parseSignature = do
  reservedOp "::"
  t <- aType
  constraints <- liftM (concat . maybeToList) $ 
                    optionMaybe $ do
                      reserved "where"
                      sepBy parseConstraint comma
  reservedOp ";"
  return $ TypeSig constraints t

parseConstraint :: Parser TypeConstraint
parseConstraint = do
  name <- lowerIdentifier
  reservedOp "~"
  overloads <- sepBy aType (reservedOp "|")
  return $ TypeConstraint name overloads

parseVarDef :: Parser (Definition ())
parseVarDef = do
  pos <- getPosition
  typeSig <- optionMaybe parseSignature
  reserved "var"
  name <- identifier
  return $ TopVarDef { topStmtPos = pos
                     , topStmtNodeData = ()
                     , varDef = VarDef { varDefPos = pos
                                       , varDefNodeData = ()
                                       , varName = name
                                       }
                     , sig = typeSig
                     }

parseFuncDef :: Parser (Definition ())
parseFuncDef = do
  pos <- getPosition
  typeSig <- optionMaybe parseSignature
  reserved "def"
  name <- identifier
  params <- parens $ sepBy parseParam comma
  body <- parseStatement
  return $ FuncDef { topStmtPos = pos
                   , topStmtNodeData = ()
                   , funcName = name
                   , funcParams = params
                   -- funcRet is just for convenience. It provides
                   -- easy access to the return type.
                   , funcRet = VarDef { varDefPos = pos
                                      , varDefNodeData = ()
                                      , varName = returnId
                                      }
                   , funcStmt = body
                   , sig = typeSig
                   }

parseForeign :: Parser (Definition ())
parseForeign = do
  pos <- getPosition
  typeSig <- optionMaybe parseSignature
  reserved "foreign"
  name <- identifier
  return $ Foreign { topStmtPos = pos
                   , topStmtNodeData = ()
                   , foreignName = "Builtin." ++ name
                   , sig = typeSig
                   }


-- #TODO unfinished (missing most useful typedefs)

--parseTypeDef :: Parser (Definition ())
--parseTypeDef = do
--  pos <- getPosition
--  reserved "type"
--  name <- typeIdentifier
--  parse

parseParam :: Parser (VarDef ())
parseParam = do
  pos <- getPosition
  name <- identifier
  return $ VarDef { varDefPos = pos
                  , varDefNodeData = ()
                  , varName = name
                  }

parseStatement :: Parser (Statement ())
parseStatement = parseAssign
             <|> parseIf
             <|> parseWhile
             <|> parseExprStmt
             <|> parseReturn
             <|> parseBlock
             <?> "statement"

parseAssign :: Parser (Statement ())
parseAssign = do
  pos <- getPosition
  name <- identifier
  reservedOp "="
  expr <- parseExpr
  return $ Assign { stmtPos = pos
                  , stmtNodeData = ()
                  , assignName = name
                  , assignExpr = expr
                  }

parseIf :: Parser (Statement ())
parseIf = do
  pos <- getPosition
  reserved "if"
  cond <- parseExpr
  thenStmt <- parseStatement
  elseStmt <- optionMaybe $ do
    reserved "else"
    parseStatement
  return $ If { stmtPos = pos
              , stmtNodeData = ()
              , ifCond = cond
              , ifThen = thenStmt
              , ifElse = elseStmt
              }

parseWhile :: Parser (Statement ())
parseWhile = do
  pos <- getPosition
  reserved "while"
  cond <- parseExpr
  stmt <- parseStatement
  return $ While { stmtPos = pos
                 , stmtNodeData = ()
                 , whileCond = cond
                 , whileBody = stmt
                 }

-- #TODO change the name of this to CallStmt
parseExprStmt :: Parser (Statement ())
parseExprStmt = do
  pos <- getPosition
  expr <- parseCall
  return $ ExprStmt { stmtPos = pos
                    , stmtNodeData = ()
                    , stmtExpr = expr
                    }

parseReturn :: Parser (Statement ())
parseReturn = do
  pos <- getPosition
  reserved "return"
  expr <- parseExpr
  return $ Return { stmtPos = pos
                  , stmtNodeData = ()
                  , returnExpr = expr
                  }

parseBlock :: Parser (Statement ())
parseBlock = do
  pos <- getPosition
  reservedOp ":"
  topStmts <- many parseDefinition
  stmts <- many parseStatement
  reserved "end"
  return $ Block { stmtPos = pos
                 , stmtNodeData = ()
                 , blockBody = (topStmts, stmts)
                 }


parseExpr :: Parser (Expr ())
parseExpr = do
  Operators opList <- getState
  let opTable = fmap (fmap fst) $ groupBy ((==) `on` snd) $ reverse $ sortBy (compare `on` snd) opList
  --let opTable = [ op | (op, prec) <- opList, then reverse ... sortWith by prec, then group by prec]
  buildExpressionParser opTable parseSmplExpr <?> "expression"


parseSmplExpr :: Parser (Expr ())
parseSmplExpr = parseIdOrCall
            <|> parens parseExpr
            <|> parseLiteral
            <?> "simple expression"

parseLiteral :: Parser (Expr ())
parseLiteral = do
  pos <- getPosition
  lit <- lexeme aLiteral
  return $ Literal { exprPos = pos
                   , exprNodeData = ()
                   , literal = lit
                   }

parseCall :: Parser (Expr ())
parseCall = do
  expr <- parseIdOrCall
  case expr of
    Call {} -> return expr
    _       -> parserZero <?> "function call"

parseIdOrCall :: Parser (Expr ())
parseIdOrCall = do
  pos <- getPosition
  name <- identifier
  args <- optionMaybe $ parens $ sepBy parseExpr comma
  let ident = Identifier { exprPos = pos
                         , exprNodeData = ()
                         , idName = name
                         , opFormat = NotOperator
                         }
  return $ case args of
    Nothing -> ident
    Just xs -> Call { exprPos = pos
                    , exprNodeData = ()
                    , callFunc = ident
                    , callParams = xs
                    }
-- #TODO figure out what I really want to do here
--parseIdentifier :: Parser String
--parseIdentifier = identifier
--              <|> brackets operator


{-
The syntax goes like this:
infixl + add 5

The function is a bit of a mind bender due to the wierdness
 of the Parsec Operator type. I written an equivalent to this 
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


readStdInt :: Num a => a -> String -> a
readStdInt base str = val
  where
    [(val,_)] = readInt base validDigit digitValue str
    digits = ['0'..'9'] ++ ['A'..'F']
    values = [0..]
    validDigit = (`elem` digits)
    digitValue x = fromJust $ lookup x (zip digits values)

aStringLiteral :: Parser Literal
aStringLiteral = do
  _ <- char '"'
  s <- many $ noneOf "\""
  _ <- char '"'

  return $ StringLiteral s

aCharLiteral :: Parser Literal
aCharLiteral = do
  _ <- char '\''
  c <- noneOf "\'"
  _ <- char '\''

  return $ CharLiteral c

aFloatLiteral :: Parser Literal
aFloatLiteral = do
  whole <- many1 $ oneOf ['0'..'9']
  d     <- char '.'
  frac  <- many1 $ oneOf ['0'..'9']

  let str = whole ++ (d : frac)
  return $ FloatLiteral str (read str)

aNumericLiteral :: Char -> [Char] -> (Integer -> Literal) -> Parser Literal
aNumericLiteral prefix digits cons = do
  _ <- char '0'
  _ <- char prefix
  lit <- many1 $ oneOf digits
  return $ cons $ readStdInt (fromIntegral $ length digits) lit

aBinLiteral :: Parser Literal
aBinLiteral = aNumericLiteral 'b' ['0'..'1'] BinLiteral

aHexLiteral :: Parser Literal
aHexLiteral = aNumericLiteral 'x' (['0'..'9'] ++ ['A'..'F']) HexLiteral

aOctalLiteral :: Parser Literal
aOctalLiteral = aNumericLiteral 'o' ['0'..'7'] OctalLiteral

aDecLiteral :: Parser Literal
aDecLiteral = do
  lit <- many1 $ oneOf ['0'..'9']
  return $ IntegerLiteral $ readStdInt 10 lit


aLiteral :: Parser Literal
aLiteral = try aStringLiteral
       <|> try aCharLiteral
       <|> try aFloatLiteral
       <|> try aBinLiteral
       <|> try aHexLiteral
       <|> try aOctalLiteral
       <|>     aDecLiteral






{-#Language GeneralizedNewtypeDeriving
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.AST where

import Data.Foldable
import Data.Traversable

import Text.Parsec.Pos
import Text.PrettyPrint

import Language.TheExperiment.Type

data Literal = StringLiteral String
             | CharLiteral Char
             | IntegerLiteral Integer
             | HexLiteral Integer
             | OctalLiteral Integer
             | FloatLiteral Float
    deriving (Show, Eq, Ord)
-- #TODO replace this with something else in Type.hs?
data ParsedType = ParsedType { typePos    :: SourcePos
                             , parsedType :: PolyType ParsedType
                             }
    deriving (Show, Eq, Ord)

data Expr a = Call              { exprPos      :: SourcePos
                                , exprNodeData :: a
                                , callFunc     :: Expr a
                                , callParams   :: [Expr a]
                                }
            | Identifier        { exprPos      :: SourcePos
                                , exprNodeData :: a
                                , idName       :: String
                                }
            | Literal           { exprPos      :: SourcePos
                                , exprNodeData :: a
                                , literal      :: Literal
                                }
            | Member            { exprPos      :: SourcePos
                                , exprNodeData :: a
                                , memberExpr   :: Expr a
                                , memberName   :: String
                                }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data Statement a = Assign   { stmtPos      :: SourcePos
                            , stmtNodeData :: a
                            , assignName   :: String
                            , assignExpr   :: Expr a
                            }
                 | If       { stmtPos      :: SourcePos
                            , stmtNodeData :: a
                            , ifCond       :: Expr a
                            , ifThen       :: Statement a
                            , ifElse       :: Maybe (Statement a)
                            }
                 | While    { stmtPos      :: SourcePos
                            , stmtNodeData :: a
                            , whileCond    :: Expr a
                            , whileBody    :: Statement a
                            }
                 | ExprStmt { stmtPos      :: SourcePos
                            , stmtNodeData :: a
                            , stmtExpr     :: Expr a
                            }
                 | Return   { stmtPos      :: SourcePos
                            , stmtNodeData :: a
                            , returnExpr   :: Expr a
                            }
                 | Block    { stmtPos      :: SourcePos
                            , stmtNodeData :: a
                            , blockBody    :: ([TopLevelStmt a], [Statement a])
                            }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data TopLevelStmt a = VarDef   { topStmtPos      :: SourcePos
                               , topStmtNodeData :: a
                               , varName         :: String
                               , varInitExpr     :: Maybe (Expr a)
                               }
                    | ConstDef { topStmtPos      :: SourcePos
                               , topStmtNodeData :: a
                               , constName       :: String
                               , constExpr       :: Expr a
                               }
                    | FuncDef  { topStmtPos      :: SourcePos
                               , topStmtNodeData :: a
                               , funcName        :: String
                               , funcParams      :: [String]
                               , funcExpr        :: Expr a
                               }
                    | TypeDef  { topStmtPos      :: SourcePos
                               , topStmtNodeData :: a
                               , typeDefName     :: String
                               , typeDefType     :: ParsedType
                               }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
                    

data Module a = Module [TopLevelStmt a]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


pPrintPos :: SourcePos -> Doc 
pPrintPos pos = text (sourceName pos) <> colon <> int (sourceLine pos) <> colon <> int (sourceColumn pos)

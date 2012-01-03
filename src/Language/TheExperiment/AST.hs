{-#Language GeneralizedNewtypeDeriving
          #-}
module Language.TheExperiment.AST where

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

data NodeType a => Expr a
            = Call              { exprPos      :: SourcePos
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
    deriving (Show, Eq, Ord)

data NodeType a => Statement a
                 = Assign   { stmtPos      :: SourcePos
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
    deriving (Show, Eq, Ord)

data NodeType a => TopLevelStmt a
                    = VarDef   { topStmtPos      :: SourcePos
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
                               , funcStmt        :: Statement a
                               }
                    | TypeDef  { topStmtPos      :: SourcePos
                               , topStmtNodeData :: a
                               , typeDefName     :: String
                               , typeDefType     :: ParsedType
                               }
    deriving (Show, Eq, Ord)
                    

data NodeType a => Module a = Module [TopLevelStmt a]
    deriving (Show, Eq, Ord)


pPrintPos :: SourcePos -> Doc 
pPrintPos pos = text (sourceName pos) <> colon <> int (sourceLine pos) <> colon <> int (sourceColumn pos)

class NodeType a where
-- nothing, just a second step to make sure you
-- are really intending to allow the type to be
-- used as a NodeType.

{-#Language GeneralizedNewtypeDeriving #-}
module Language.TheExperiment.AST where

import Text.Parsec.Pos

import Language.TheExperiment.Type

data Literal = StringLiteral String
             | CharLiteral Char
             | IntegerLiteral Integer
             | HexLiteral Integer
             | OctalLiteral Integer
             | FloatLiteral Double
    deriving (Show, Eq, Ord)

newtype NodeId = NodeId Int -- unique identifier for each node in the AST
    deriving (Show, Eq, Ord, Enum, Bounded, Num, Integral, Real)

data ParsedType = ParsedType { typePos    :: SourcePos
                             , parsedType :: Type ParsedType
                             }
    deriving (Show, Eq, Ord)

-- #TODO missing elements: Pointer member access (do we want?), Array index?
data Expr = Call              { exprPos      :: SourcePos
                              , exprNodeId   :: NodeId
                              , callFunc     :: Expr
                              , callParams   :: [Expr]
                              }
          | Identifier        { exprPos      :: SourcePos
                              , exprNodeId   :: NodeId
                              , idName       :: String
                              }
          | Literal           { exprPos      :: SourcePos
                              , exprNodeId   :: NodeId
                              , literal      :: Literal
                              }
          | Cast              { exprPos      :: SourcePos
                              , exprNodeId   :: NodeId
                              , castType     :: ParsedType
                              , castExpr     :: Expr
                              }
          | Member            { exprPos      :: SourcePos
                              , exprNodeId   :: NodeId
                              , memberExpr   :: Expr
                              , memberName   :: String
                              }
    deriving (Show, Eq, Ord)


data Statement = Assign   { stmtPos    :: SourcePos
                          , stmtNodeId :: NodeId
                          , assignName :: String
                          , assignExpr :: Expr
                          }
               | VarDef   { stmtPos    :: SourcePos
                          , stmtNodeId :: NodeId
                          , varName    :: String
                          }
               | If       { stmtPos    :: SourcePos
                          , stmtNodeId :: NodeId
                          , ifCond     :: Expr
                          , ifThen     :: Statement
                          , ifElse     :: Maybe Statement
                          }
               | While    { stmtPos    :: SourcePos
                          , stmtNodeId :: NodeId
                          , whileCond  :: Expr
                          , whileBody  :: Statement
                          }
               | FuncDef  { stmtPos    :: SourcePos
                          , stmtNodeId :: NodeId
                          , funcName   :: String
                          , funcParams :: [String]
                          , funcExpr   :: Expr
                          }
               | ExprStmt { stmtPos    :: SourcePos
                          , stmtNodeId :: NodeId
                          , stmtExpr   :: Expr
                          }
               | Block    { stmtPos    :: SourcePos
                          , stmtNodeId :: NodeId
                          , blockBody  :: [Statement]
                          }
    deriving (Show, Eq, Ord)

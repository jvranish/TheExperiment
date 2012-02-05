{-#Language GeneralizedNewtypeDeriving
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.AST where

import Text.Parsec.Pos
import Text.PrettyPrint

import Data.Foldable
import Data.Traversable

import Language.TheExperiment.Type
-- import Language.TheExperiment.CodeGenType -- Only used for type constraint not
                                          --  actually needed

data Literal = StringLiteral String
             | CharLiteral Char
             | IntegerLiteral Integer
             | BinLiteral Integer
             | HexLiteral Integer
             | OctalLiteral Integer
             | FloatLiteral String Double -- String is parsed representation.
    deriving (Show, Eq, Ord)
-- #TODO replace this with something else in Type.hs?

data ParsedType = TypeName   { -- Int, Var, Foo, Void
                  typePos      :: SourcePos,
                  typeName     :: String
                } |
                TypeVariable { -- a, b, c, d, bees
                  typePos      :: SourcePos,
                  typeVariable :: String
                } |
                TypeCall     { -- Foo a Int, Foo a b, Foo (Foo Var)
                  typePos      :: SourcePos,
                  typeFunction :: ParsedType,
                  typeParams   :: [ParsedType]
                } |
                Function     { -- (a, b) -> c
                  typePos      :: SourcePos,
                  argTypes     :: [ParsedType],
                  returnType   :: ParsedType
                }
  deriving (Show, Eq, Ord)

data Expr a
        = Call       { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , callFunc     :: Expr a
                     , callParams   :: [Expr a]
                     }
        | Identifier { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , idName       :: String
                     }
        | Literal    { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , literal      :: Literal
                     }
        {-
        | Member     { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , memberExpr   :: Expr a
                     , memberName   :: String
                     }
        -}
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data Statement a
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
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data VarDef a 
        = VarDef { varDefPos      :: SourcePos -- this pos
                  -- is the position of the var name
                 , varDefNodeData :: a
                 , varName        :: String
                 }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data TopLevelStmt a
        = TopVarDef { topStmtPos      :: SourcePos
                    , topStmtNodeData :: a
                    , varDef          :: VarDef a
                    -- , varInitExpr     :: Maybe (Expr a)
                    }
        | FuncDef   { topStmtPos      :: SourcePos
                    , topStmtNodeData :: a
                    , funcName        :: String
                    , funcParams      :: [VarDef a]
                    , funcRet         :: VarDef a
                    , funcStmt        :: Statement a
                    }
        | TypeDef   { topStmtPos      :: SourcePos
                    , topStmtNodeData :: a
                    , typeDefName     :: String
                    , typeDefType     :: TypeDef ParsedType
                    }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
                    

data Module a = Module SourcePos [TopLevelStmt a]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


pPrintPos :: SourcePos -> Doc 
pPrintPos pos = text (sourceName pos) <> colon <> int (sourceLine pos) <> colon <> int (sourceColumn pos)

class NodeType a where
-- nothing, just a second step to make sure you
-- are really intending to allow the type to be
-- used as a NodeType.
-- instance NodeType GenType where

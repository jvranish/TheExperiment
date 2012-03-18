{-#Language GeneralizedNewtypeDeriving
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.AST.Statement where

import Text.Parsec.Pos

import Data.Foldable
import Data.Traversable

import Language.TheExperiment.AST.Expression
import Language.TheExperiment.AST.Type


data Definition a = TypeDef { defnPos      :: SourcePos
                            , defnNodeData :: a
                            , typeDefName  :: String
                            , typeDefType  :: ParsedType
                            }
                  | TypeSignature { defnPos      :: SourcePos
                                  , defnNodeData :: a
                                  , typeSigNames :: [String]
                                  , typeSigType  :: ParsedType
                                  }
                  | VariableDef { defnPos         :: SourcePos
                                , defnNodeData    :: a
                                , variableDefName :: VarDef a
                                -- , initializationExpr :: Maybe (Expr a)
                                }
                  | ForeignDef { defnPos       :: SourcePos
                               , defnNodeData  :: a
                               , nativeDefName :: String
                               , foreignName   :: String
                               , nativeDefType :: ParsedType
                               } -- foreign cFunction "c_function" (Int -> Int)
                  | FunctionDef { defnPos           :: SourcePos
                                , defnNodeData      :: a
                                , functionName      :: String
                                , functionParams    :: [VarDef a]
                                , funcRet           :: VarDef a
                                , functionStatement :: Statement a
                                }
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
                   , blockBody    :: ([Definition a], [Statement a])
                   }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


data VarDef a  = VarDef { varDefPos      :: SourcePos -- this pos
                         -- is the position of the var name
                        , varDefNodeData :: a
                        , varName        :: String
                        }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

returnId :: String
returnId = "#return"

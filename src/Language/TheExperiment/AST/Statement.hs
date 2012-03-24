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
                  | DefSignature { defnPos      :: SourcePos
                                 , defnNodeData :: a
                                 , typeSigNames :: [String]
                                 , typeSigType  :: TypeSignature
                                 }
                  | VariableDef { defnPos      :: SourcePos
                                , defnNodeData :: a
                                , variable     :: Variable a
                                -- , initializationExpr :: Maybe (Expr a)
                                }
                  | ForeignDef { defnPos       :: SourcePos
                               , defnNodeData  :: a
                               , nativeDefName :: String
                               , foreignName   :: String
                               } -- foreign cFunction "c_function"
                  | FunctionDef { defnPos        :: SourcePos
                                , defnNodeData   :: a
                                , functionName   :: String
                                , functionParams :: [Variable a]
                                , functionRet    :: a
                                , functionBlock  :: RawBlock a
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
                   , ifThen       :: RawBlock a
                   , ifElse       :: Maybe (RawBlock a) -- RawBlock || If in the case of elif
                   }
        | While    { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , whileCond    :: Expr a
                   , whileBody    :: Statement a
                   }
        | CallStmt { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , stmtExpr     :: Expr a
                   }
        | Return   { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , returnExpr   :: Expr a
                   }
        | Block    { stmtPos      :: SourcePos
                   , stmtNodeData :: a
                   , rawBlock     :: RawBlock a
                   }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data RawBlock a = RawBlock { blockPos      :: SourcePos
                           , blockNodeData :: a
                           , blockBody    :: [DefOrStatement a]
                           }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data DefOrStatement a = Def (Definition a)
                      | Stmt (Statement a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

defOrStmtToEither :: DefOrStatement a 
                  -> Either (Definition a) (Statement a)
defOrStmtToEither (Def a) = Left a
defOrStmtToEither (Stmt a) = Right a


data Variable a  = Variable { varDefPos      :: SourcePos -- this pos
                             -- is the position of the var name
                            , varDefNodeData :: a
                            , varName        :: String
                            }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

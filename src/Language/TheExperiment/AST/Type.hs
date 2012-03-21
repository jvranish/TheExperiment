
{-#Language GeneralizedNewtypeDeriving
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.AST.Type where

import Text.Parsec.Pos

import Data.Foldable
import Data.Traversable

data TypeSignature = TypeSignature [TypeConstraint] ParsedType
  deriving (Show, Ord, Eq)

data TypeConstraint = TypeConstraint String [ParsedType]
  deriving (Show, Ord, Eq)


data ParsedType = TypeName     { typePos  :: SourcePos
                               , typeName :: String
                               }  -- Int, Var, Foo, Void 
                | TypeVariable { typePos     :: SourcePos
                               , typeVarName :: String
                               } -- a, b, c, d, bees
                | TypeCall     { typePos      :: SourcePos
                               , typeFunction :: ParsedType
                               , typeParams   :: [ParsedType]
                               } -- Foo a Int, Foo a b, Foo (Foo Var)
                | FunctionType { typePos    :: SourcePos
                               , typeArgs   :: [ParsedType]
                               , returnType :: ParsedType
                               }
 deriving (Show, Eq, Ord)



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


data Type a = TypeName     { typeName :: String
                           }  -- Int, Var, Foo, Void 
            | TypeVariable { typeVarName     :: Maybe String
                           , typeConstraints :: [a]
                           } -- a, b, c, d, bees
            | TypeCall     { typeFunction :: a
                           , typeParams   :: [a]
                           } -- Foo a Int, Foo a b, Foo (Foo Var)
            | FunctionType { typeArgs   :: [a]
                           , returnType :: a
                           }
 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data ParsedType = ParsedType { typePos    :: SourcePos
                             , parsedType :: Type ParsedType
                             }
 deriving (Show, Ord)

-- This will ignore differences in SourcePos (this makes testing easier)
instance Eq ParsedType where
  ParsedType _ a == ParsedType _ b = a == b



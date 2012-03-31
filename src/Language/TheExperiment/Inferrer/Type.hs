{-#Language DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.Inferrer.Type where

import Language.TheExperiment.CodeGen.Type

import Data.Foldable
import Data.Traversable
import qualified Data.Map as Map

data Type a = TypeName String
            | Std StdType
            | Pointer a
            | Array a a -- first parameter should only be a type variable or 
                        -- an NType (type level number), eventually this would 
                        -- be moved to the std libs, and have a constraint on
                        -- that parameter
            | NType Integer
            | Func [a] a
            {-
            | Pointer a
            | 
            | Array (Maybe Integer) a
            | TCall a [a]
            | Struct [(String, a)]
            | Union [(String, a)]
            | NType Integer
            | Func [a] a
            -}
            -- optional name, list of record fields and their types (empty for
            --  non-structures), Just overloads
            | Var (Maybe String) {- (RecFields a) -} [a]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data RecFields a = Fields (Map.Map String a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

noRecFields :: RecFields a
noRecFields = Fields Map.empty

-- #TODO consider making this just a list
--   I think it would simplify the logic in many places
--  (perhaps just a newtype'd list)

newtype Overloads a = Overloads [a] -- potential types
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data FlatType = FlatType (Type FlatType)
    deriving (Show, Eq, Ord)

-- the type variable 'a' is meant to be either a TypeRef or some other version
-- of Type (like say data FlatType = FlatType (Type FlatType))
data TypeDef a = SimpleType a
               | AlgType [(String, a)]
               | Struct [(String, a)]
               | Union [(String, a)]
    deriving (Show, Eq, Ord)


{-#Language DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.Type where

import Data.Foldable
import Data.Traversable
import qualified Data.Map as Map

data StdType = Void | Char8 | IntType IntType | SBool | F32 | F64
    deriving (Show, Eq, Ord)

data IntType = Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Int64 | UInt64
    deriving (Show, Eq, Ord, Bounded, Enum)

isValidInt :: IntType -> Integer -> Bool
isValidInt t n | n >= minValue t && n <= maxValue t = True
isValidInt _ _ = False

maxValue :: IntType -> Integer
maxValue Int8 = 2^(7 :: Integer) - 1
maxValue UInt8 = 2^(8 :: Integer) - 1
maxValue Int16 = 2^(15 :: Integer) -1
maxValue UInt16 = 2^(16 :: Integer) - 1
maxValue Int32 = 2^(31 :: Integer) - 1
maxValue UInt32 = 2^(32 :: Integer) - 1
maxValue Int64 = 2^(63 :: Integer) - 1
maxValue UInt64 = 2^(64 :: Integer) - 1

minValue :: IntType -> Integer
minValue Int8 = -2^(7 :: Integer)
minValue UInt8 = 0
minValue Int16 = -2^(15 :: Integer)
minValue UInt16 = 0
minValue Int32 = -2^(31 :: Integer)
minValue UInt32 = 0
minValue Int64 = -2^(63 :: Integer)
minValue UInt64 = 0

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
            | Var (Maybe String) {- (RecFields a) -} (Overloads a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data RecFields a = Fields (Map.Map String a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

noRecFields :: RecFields a
noRecFields = Fields Map.empty

data Overloads a = NotOverloaded
                 | Overloads a [a] -- current best guess, potential types
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data FlatType = FlatType (Type FlatType)
    deriving (Show, Eq, Ord)

-- the type variable 'a' is meant to be either a TypeRef or some other version
-- of Type (like say data FlatType = FlatType (Type FlatType))
data TypeDef a = SimpleType a
               | AlgType [(String, a)]
               | Struct [(String, a)]
               | Union [(String, a)]



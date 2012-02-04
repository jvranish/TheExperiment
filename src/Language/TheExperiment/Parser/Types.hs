module Language.TheExperiment.Parser.Types (
  Type,
) where

data Type = CompositeType String [Type]
          | KindedType KindedType
          | TypeVariable TypeVariable
          | NumericType Int
          | StructuredType String [(String, Type)]
          | Function [Type] Type
  deriving (Show)

data KindedType = KindedType [TypeVariable] Type
  deriving (Show)

data TypeVariable = TypeVariable String
  deriving (Show)



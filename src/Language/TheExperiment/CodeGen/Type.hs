module Language.TheExperiment.CodeGen.Type where

data GenBasicType = CTypeName String
                  | CStd StdType
    deriving (Show, Eq, Ord)

data GenType = GenType GenBasicType [GenTypeDerivDecl]
    deriving (Show, Eq, Ord)

data GenTypeDerivDecl = CArrayDecl [GenTypeQualifier] Integer
                      | CPointerDecl [GenTypeQualifier]
    deriving (Show, Eq, Ord)

data GenTypeQualifier = CImmutable
                      | CVolatile
    deriving (Show, Eq, Ord)

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


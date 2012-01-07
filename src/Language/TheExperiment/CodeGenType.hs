module Language.TheExperiment.CodeGenType where

import Language.TheExperiment.Type

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


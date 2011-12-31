module Language.TheExperiment.Type where

data StdType = Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Int64 | UInt64 | SBool | F32 | F64
  deriving (Show, Eq, Ord, Bounded, Enum)


data Type a = TypeName String
            | Std StdType
            {-
            | Pointer a
            | AlgType [(String, a)]
            | Array (Maybe Integer) a
            | TCall a [a]
            | Struct [(String, a)]
            | Union [(String, a)]
            | NType Integer
            | Func [a] a
            -}
            | Var (Maybe String) [(String, a)] (Maybe [a]) -- optional name, list of record fields and their types (empty for non-structures), Just overloads
    deriving (Show, Eq, Ord)



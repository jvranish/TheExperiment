module Language.TheExperiment.Parser.Types (
  Type,
) where

-- data Type = CompositeType String [Type]
--           | KindedType KindedType -- (Foo a) => a Int
--           | TypeVariable TypeVariable
--           | NumericType Int
--           | StructuredType String [(String, Type)]
--           | Function [Type] Type
--   deriving (Show)

-- data KindedType = KindedType [TypeVariable] Type
--   deriving (Show)
-- 
-- data TypeVariable = TypeVariable String
--   deriving (Show)

data Type = TypeName String -- Int, Var, Foo, Void
          | TypeCall Type [Type] -- Foo a Int, Foo a b, Foo (Foo Var)
          | TypeVariable String -- a, b, c, d, bees
          | Function [Type] Type -- (a, b) -> c
  deriving (Show)


-- foo :: (Int, a || {zap :: Int, boop :: Int}, Bar) -> c
-- foo(num, a, thing):
--   case thing of
--     1 -> num + a.zap
--     2 -> num + a.boop
-- 
-- 
-- foo (b , a | {zap :: Int, boop :: b}) => (Int, a, Bar) -> c
-- 
-- b | {Int | Float | zap :: Int, boop :: b}
-- 
-- foo ||
--   (a, b, c) -> d |
--     a :: Int
--     b :: {x, y}
--     c




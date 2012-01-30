Ref
Ptr
MemAddr
(a) -> Ref a
(Ptr a) -> Ref a
MemAddr -> Ptr a
Ref a -> a
Ptr a -> a

Can't return a ref to stack variables defined on this stack.


data TypeVariable
	TypeVariable String

data Type
	CompositeType String [Type]
	TypeVariable TypeVariable
	NumericType Int
	Structure [(String, Type)]
  Function [Type] Type

KindedType
	KindedType [TypeVariable] Type

a -> Int

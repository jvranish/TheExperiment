Ref
Ptr
MemAddr
(a) -> Ref a
(Ptr a) -> Ref a
MemAddr -> Ptr a
Ref a -> a
Ptr a -> a

Can't return a ref to stack variables defined on this stack.


data Type
	CompositeType String [Type]
  KindedType KindedType
	TypeVariable TypeVariable
	NumericType Int
	Structure String [(String, Type)]
  Function [Type] Type

KindedType
	KindedType [TypeVariable] Type

data TypeVariable
	TypeVariable String

-- Assignment

x = 5 // instantiate
x = 6 // reassign
y = ref(x) // instantiate reference to x
y := 9 // update the thing y references to 9
       // (:=) is a function (Ref a) -> a -> (Ref a)

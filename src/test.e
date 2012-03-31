
infixl + add 4
infixl - sub 4
infixl * mul 8

infixl == eq 1

add, sub, mul :: UInt32, UInt32 -> UInt32
foreign add "bla"
foreign sub "bla"
foreign mul "bla"

eq :: UInt32, UInt32 -> Bool
foreign eq "bla"

def fact(a):
  if a == 0:
    return 1
  else:
    return a * fact(a-1)

def main(a):
  var x
  x = 9
  return fact(x)


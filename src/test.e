
infixl + add 4
infixl - sub 4
infixl * mul 8

infixl == eq 1

:: UInt32, UInt32 -> UInt32;
foreign add

:: UInt32, UInt32 -> UInt32;
foreign sub

:: UInt32, UInt32 -> UInt32;
foreign mul

:: UInt32, UInt32 -> Bool;
foreign eq

def fact(a):
  if a == 0:
    return 1
  end
  else:
    return a * fact(a-1)
  end
end

def main():
  var x
  x = 9
  return fact(x)
end


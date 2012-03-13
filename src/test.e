
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

// this does not work yet
:: UInt32 -> UInt32;
def fact(a):
  if a == 0:
    return 1
  end
  else:
    return a * fact(a-1)
  end
end

:: UInt32 -> UInt32;
def main(z):
  :: UInt32;
  var x
  x = 9
  return fact(x)
end


// main, foo, bar :: UInt32
// main:
//   var x
//   x = 9
//   return fact(x)

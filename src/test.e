
infixl + add 4
infixl - sub 4
infixl * mul 8

infixl == eq 1

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

def main():
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

module Main where

import Language.TheExperiment.Parser

main :: IO ()
main = do
  print $ eParser "type Foo = Bar"
  print $ eParser "type Foo = Bar a"
  print $ eParser "var x"
  print $ eParser "x :: Int"
  print $ eParser "x,y , z :: Int"
  print $ eParser "foreign cFoo \"c_foo\" (Int -> Int)"
  print $ eParser "foo:"
  print $ eParser "foo(a,b):"

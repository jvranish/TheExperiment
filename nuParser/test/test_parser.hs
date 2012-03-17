module Main where

import Language.TheExperiment.Parser

main :: IO ()
main = do
  normal

normal :: IO ()
normal = do
  print $ eParser "type Foo = Bar"
  print $ eParser "type Foo = Bar a"
  print $ eParser "var x"
  print $ eParser "x :: Int"
  print $ eParser "x,y , z :: Int"
  print $ eParser "foreign cFoo \"c_foo\" (Int -> Int)"
  print $ eParser $ unlines ["def foo(a,b): var x",
                             "              var y"]
  print $ eParser $ unlines ["def foo(a,b):",
                             "  def bar(x,y):",
                             "    var a",
                             "    var b",
                             "  var c",
                             "  return 9"]

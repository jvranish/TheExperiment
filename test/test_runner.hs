module Main (main) where

import qualified ETests.Parser

failures :: [(Bool,a,b,c)] -> [(Bool,a,b,c)]
failures = filter (not . (\(x,_,_,_) -> x))

show_test :: (Show a, Show b) => (Bool, String, a, b) -> String
show_test (comparison, input, expected, actual) =
  case comparison of
    True  -> "  -- " ++ input ++ " -> " ++ show actual
    False -> "F -- " ++ input ++ " :: " ++ show expected ++ " =/= " ++ show actual

main :: IO ()
main = do
  let the_fails = failures ETests.Parser.test

  case length the_fails of
     0 -> putStrLn "No fails! :D"
     x -> do
      putStrLn $ (show x) ++ " fails. :("
      putStrLn $ unlines $ map show_test $ ETests.Parser.test


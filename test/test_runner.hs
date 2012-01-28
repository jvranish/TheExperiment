module Main (main) where

import qualified ETests.Parser

failures :: [(Bool,String,String)] -> [(Bool,String,String)]
failures = filter (not . (\(x,_,_) -> x))

show_test :: (Bool, String, String) -> String
show_test (comparison, parseName, message) =
  case comparison of
    True  -> "  -- " ++ parseName ++ " -> " ++ message
    False -> "F -- " ++ parseName ++ " :: " ++ message

main :: IO ()
main = do
  let the_fails = failures ETests.Parser.parser_test

  case length the_fails of
     0 -> putStrLn "No fails! :D"
     x -> do
      putStrLn $ (show x) ++ " fails. :("
      putStrLn $ unlines $ map show_test $ the_fails
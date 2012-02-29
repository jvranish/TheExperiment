module Main where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.AST
import Language.TheExperiment.PrettyPrint


main :: IO ()
main = do
  -- sample (arbitrary :: Gen ParsedType)

  samples <- sample' (arbitrary :: Gen ParsedType)
  mapM_ putStrLn $ (map (render . ppParsedType) samples)

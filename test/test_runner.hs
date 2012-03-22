module Main (main) where

import ETests.Test

import Test.Hspec

main :: IO ()
main = hspecX eSpecs

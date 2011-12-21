module Main (main) where

import Test.Hspec.Runner
import System.Exit

import qualified Language.TheExperiment.CodeGenSpec as CodeGenSpec
import qualified Language.TheExperiment.LexerSpec as LexerSpec

specRunners :: [IO Bool]
specRunners = [ CodeGenSpec.main
              , LexerSpec.main
              ]

main :: IO ()
main = exitWith =<< fmap (toExitCode . and) (sequence specRunners)
    
module ETests.Test
  ( module ETests.Parser.Type
  , module ETests.Parser.Literal
  -- , module ETests.Parser.Statement
  , module ETests.Pretty.Type
  , eSpecs
  ) where

import Test.Hspec

import ETests.Parser.Type
import ETests.Pretty.Type
import ETests.Parser.Literal

eSpecs :: Specs
eSpecs = concat $ [ aTypeSignatureSpecs
                  , aTypeSpecs
                  , prettyTypeSignatureSpecs
                  , prettyTypeSpecs
                  , aLiteralSpecs
                  ]
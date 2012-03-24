module ETests.Test
  ( module ETests.Parser.Type
  , module ETests.Parser.Literal
  , module ETests.Parser.Expression
  , module ETests.Pretty.Type
  , eSpecs
  ) where

import Test.Hspec

import ETests.Parser.Type
import ETests.Pretty.Type
import ETests.Parser.Literal
import ETests.Parser.Expression

eSpecs :: Specs
eSpecs = concat $ [ aTypeSignatureSpecs
                  , aTypeSpecs
                  , prettyTypeSignatureSpecs
                  , prettyTypeSpecs
                  , aLiteralSpecs
                  , anExprSpecs
                  ]
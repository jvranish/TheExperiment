module ETests.Test
  ( module ETests.Parser.Type
  , module ETests.Parser.Literal
  , module ETests.Parser.Expression
  , module ETests.Pretty.Type
  , module ETests.Pretty.Literal
  , module ETests.Pretty.Expression
  , eSpecs
  ) where

import Test.Hspec

import ETests.Parser.Type
import ETests.Pretty.Type
import ETests.Parser.Literal
import ETests.Pretty.Literal
import ETests.Parser.Expression
import ETests.Pretty.Expression
import ETests.Parser.Statement

eSpecs :: Specs
eSpecs = concat $ [ aTypeSignatureSpecs
                  , aTypeSpecs
                  , prettyTypeSignatureSpecs
                  , prettyTypeSpecs
                  , aLiteralSpecs
                  , prettyLiteralSpecs
                  , anExprSpecs
                  , prettyExpressionSpecs
                  , statementSpecs
                  ]

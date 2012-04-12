module ETests.Test
  ( module ETests.Parser.Type
  , module ETests.Parser.Literal
  , module ETests.Parser.Expression
  , module ETests.Pretty.Type
  , module ETests.Pretty.Literal
  , module ETests.Pretty.Expression
  , module ETests.Pretty.Statement
  , module ETests.CodeGen.Gen
  , eSpecs
  ) where

import Test.Hspec

import ETests.CodeGen.Gen
import ETests.Parser.Expression
import ETests.Parser.Literal
import ETests.Parser.Statement
import ETests.Parser.Type
import ETests.Pretty.Expression
import ETests.Pretty.Literal
import ETests.Pretty.Statement
import ETests.Pretty.Type

eSpecs :: Specs
eSpecs = concat $ [ aTypeSignatureSpecs
                  , aTypeSpecs
                  , prettyTypeSignatureSpecs
                  , prettyTypeSpecs
                  , aLiteralSpecs
                  , prettyLiteralSpecs
                  , anExprSpecs
                  , prettyExpressionSpecs
                  , prettyStatementSpecs
                  , statementSpecs
                  , genExprSpecs
                  ]

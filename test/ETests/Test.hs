module ETests.Test
  ( module ETests.Parser.Type
  -- , module ETests.Parser.Statement
  , module ETests.Pretty.Type
  , runTests
  ) where

import ETests.Parser.Type
import ETests.Pretty.Type

runTests :: IO ()
runTests = do
  testPrettyTypeSignature
  testPrettyType
  testParseTypeSignature
  testParseType
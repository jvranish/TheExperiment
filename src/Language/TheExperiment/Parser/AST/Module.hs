{-#Language DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.Parser.AST.Module where

import Text.Parsec.Pos

import Data.Foldable
import Data.Traversable

import Language.TheExperiment.Parser.AST.Statement

data Module a = Module SourcePos [Definition a]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


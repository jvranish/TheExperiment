{-#Language DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.AST.Module where

import Text.Parsec.Pos

import Data.Foldable
import Data.Traversable

import Language.TheExperiment.AST.Statement

data Module a = Module SourcePos [Definition a]
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


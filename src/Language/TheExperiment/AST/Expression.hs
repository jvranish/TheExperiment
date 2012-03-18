{-#Language GeneralizedNewtypeDeriving
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          #-}
module Language.TheExperiment.AST.Expression where

import Text.Parsec.Pos

import Data.Foldable
import Data.Traversable

data Expr a
        = Call       { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , callFunc     :: Expr a
                     , callParams   :: [Expr a]
                     }
        | Identifier { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , idName       :: String
                     , opFormat     :: OpFormat
                     }
        | Literal    { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , literal      :: Literal
                     }
        {-
        | Member     { exprPos      :: SourcePos
                     , exprNodeData :: a
                     , memberExpr   :: Expr a
                     , memberName   :: String
                     }
        -}
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data Literal = StringLiteral String
             | CharLiteral Char
             | IntegerLiteral Integer
             | BinLiteral Integer
             | HexLiteral Integer
             | OctalLiteral Integer
             | FloatLiteral String Double -- String is parsed representation.
    deriving (Show, Eq, Ord)

-- format indicator for pretty printing (keeps track of precedence, etc...)
data OpFormat = NotOperator
              | ExplicitOperator
              | In Rational
              | InR Rational
              | InL Rational
              | Pre Rational
              | Post Rational
  deriving (Show, Ord, Eq)


module Language.TheExperiment.AST where

data Literal = LiteralInt Integer
    deriving (Show, Eq, Ord)

data Expr = Call Expr [Expr]
          | Identifier String
          | ExprLit Literal
    deriving (Show, Eq, Ord)


{-
data Statement = ExprStmt Expr
               | Scope [Statement]
               | FuncDef String [String] Statement
               | Return Expr
    deriving (Show, Eq, Ord)

-}
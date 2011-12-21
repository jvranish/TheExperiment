module Language.TheExperiment.AST where

data Literal = IntegerLiteral Integer
    deriving (Show, Eq, Ord)

data Expr = Call Expr [Expr]
          | Identifier String
          | LiteralExpr Literal
    deriving (Show, Eq, Ord)


data Statement = ExprStmt Expr
               | Scope [Statement]
               | Return Expr
    deriving (Show, Eq, Ord)

data TopLevel = FuncDef String [String] Statement
    deriving (Show, Eq, Ord)
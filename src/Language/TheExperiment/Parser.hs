module Language.TheExperiment.Parser where

{- General structure of types:
 - 
 - Module
 - \
 -  TopLevelStmt
 -   \
 -    Statement
 -    \
 -     Expr
 - 
 - The type parameter to Module, TopLevelStmt, Statement, and Expr is () for
 - the parser stage. This information is filled in later by the inference
 - engine and the type checker.
 -
 -}

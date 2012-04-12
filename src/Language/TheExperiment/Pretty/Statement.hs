module Language.TheExperiment.Pretty.Statement
  ( prettyStatement
  ) where


import Text.PrettyPrint.HughesPJ
import Language.TheExperiment.Parser.AST.Statement
import Language.TheExperiment.Parser.AST.Expression
import Language.TheExperiment.Parser.Statement
import Language.TheExperiment.Pretty.Expression

prettyStatement :: ParsedStatement -> Doc
prettyStatement s =
  case s of
    (Assign _ _ name expr)   -> prettyAssign name expr
    i@(If {})       -> prettyIf       i
    w@(While {})    -> prettyWhile    w
    c@(CallStmt {}) -> prettyCallStmt c
    r@(Return {})   -> prettyReturn   r
    b@(Block {})    -> prettyBlock    b

prettyAssign :: String -> Expr a -> Doc
prettyAssign name expr = 
  text name <+> text "=" <+> prettyExpression expr

prettyIf       i = undefined
prettyWhile    w = undefined
prettyCallStmt c = undefined
prettyReturn   r = undefined
prettyBlock    b = undefined

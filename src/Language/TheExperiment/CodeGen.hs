module Language.TheExperiment.CodeGen where

import Language.C.Syntax
import Language.C.Pretty

import Text.PrettyPrint

import Language.TheExperiment.AST

genExpr :: Expr -> Doc
genExpr (ExprLit (LiteralInt n)) = pretty $ (CConst $ CIntConst (CInteger n DecRepr noFlags) undefined :: CExpr)

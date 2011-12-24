module Language.TheExperiment.CodeGen where

import Language.C.Syntax
import Language.C.Pretty
import Language.C.Data.Ident

import Text.PrettyPrint

import Language.TheExperiment.AST

-- #TODO this should throw an error for integers that don't fit in the current type
genExpr :: Expr -> Doc
genExpr (ExprLit (LiteralInt n)) = pretty $ (CConst $ CIntConst (CInteger n DecRepr noFlags) undefined :: CExpr)
genExpr (Identifier s) = pretty $ (CVar (Ident s 0 undefined) undefined :: CExpr)
genExpr (Call f params) = undefined
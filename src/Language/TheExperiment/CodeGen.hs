module Language.TheExperiment.CodeGen where

import Language.C.Syntax
import Language.C.Pretty

import Text.PrettyPrint.HughesPJ

import Language.TheExperiment.AST

genExpr :: Expr -> Doc
genExpr = undefined
module Language.TheExperiment.CodeGen where

import Text.Parsec.Pos

import Language.C.Syntax

import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position

import Text.PrettyPrint

import Language.TheExperiment.AST
import Language.TheExperiment.Error


getNodeInfo :: SourcePos -> NodeInfo
getNodeInfo pos = OnlyPos cPos (cPos, 0)
    where
        cPos = position 0 (sourceName pos) (sourceLine pos) (sourceColumn pos) 



genExpr :: Expr -> ErrorM CExpr
genExpr (Literal { exprPos = pos, literal = lit } ) = fmap CConst $ case lit of
    StringLiteral s  -> return $ CStrConst (cString s) (getNodeInfo pos) -- #TODO check that type is correct
    CharLiteral c    -> return $ CCharConst  (cChar c) (getNodeInfo pos) -- #TODO check type
    IntegerLiteral i -> return $ CIntConst (CInteger i DecRepr noFlags) (getNodeInfo pos)   -- #TODO check that type fits, and correct flags
    HexLiteral i     -> return $ CIntConst (CInteger i HexRepr noFlags) (getNodeInfo pos)  
    OctalLiteral i   -> return $ CIntConst (CInteger i OctalRepr noFlags) (getNodeInfo pos)
    FloatLiteral f   -> return $ CFloatConst (cFloat (fromRational $ toRational f)) (getNodeInfo pos)  -- #TODO check that correct type
genExpr (Identifier { exprPos = pos, idName = name } ) = return $ CVar (Ident name 0 (getNodeInfo pos)) (getNodeInfo pos) -- #TODO we'll need to update the identifierId at some point if we want to run any analysis functions on the output
genExpr (Call { exprPos = pos, callFunc = f, callParams = params } ) = case f of
    Identifier { idName = "Builtin.adduint32" } -> do
        case params of
            [a, b] -> do
                a' <- genExpr a
                b' <- genExpr b
                return $ CBinary CAddOp a' b' (getNodeInfo pos)   -- #TODO check types
            _ -> throwFatalError $ text "Invalid Number of parameters to build in operator (This should never happen, it's a compiler error)."
    
    _ ->  do
        f' <- genExpr f
        params' <- mapM genExpr params
        return $ CCall f' params' (getNodeInfo pos)

genExpr (Member { exprPos = pos, memberExpr = mExpr, memberName = name } ) = do
    mExpr' <- genExpr mExpr
    return $ CMember mExpr' (Ident name 0 (getNodeInfo pos)) False (getNodeInfo pos)

{-
genBuildinFunc name params = case name of
    "add8" -> genBinOp 
operators
arithmetic
bitwise -- 
logic -- verify boolean type
func call
cast
member

literals --check type is right size, etc...

functions
vardefs
constdef
typedef

import list, export list
modules
gen prototypes
gen typedefs

Builtin.adduint8
Builtin.adduint16
Builtin.adduint32
Builtin.adduint64

Builtin.addint8
Builtin.addint16
Builtin.addint32
Builtin.addint64

specialize - AST with flat types, no polymorphics

genCfileCode
genHfileCode 


the generators should output language.c AST, which we then pretty print
-}
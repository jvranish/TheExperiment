module Language.TheExperiment.CodeGen where

import Data.List (genericLength)

import Control.Monad

import Text.Parsec.Pos
import Text.PrettyPrint

import Language.C.Syntax
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position

import Language.TheExperiment.Type
import Language.TheExperiment.AST
import Language.TheExperiment.Error


getNodeInfo :: SourcePos -> NodeInfo
getNodeInfo pos = OnlyPos cPos (cPos, 0)
    where
        cPos = position 0 (sourceName pos) (sourceLine pos) (sourceColumn pos) 

cUnaryOpTable :: [(String, (CUnaryOp, FlatMonoType, FlatMonoType))]
cUnaryOpTable = []

cBinOpTable :: [(String, (CBinaryOp, FlatMonoType, FlatMonoType, FlatMonoType))]
cBinOpTable = [ ("Builtin.addint8", (CAddOp, flatType (Std $ IntType Int8), flatType (Std $ IntType Int8), flatType (Std $ IntType Int8)))
              , ("Builtin.adduint8", (CAddOp, flatType (Std $ IntType UInt8), flatType (Std $ IntType UInt8), flatType (Std $ IntType UInt8)))
              , ("Builtin.addint16", (CAddOp, flatType (Std $ IntType Int16), flatType (Std $ IntType Int16), flatType (Std $ IntType Int16)))
              , ("Builtin.adduint16", (CAddOp, flatType (Std $ IntType UInt16), flatType (Std $ IntType UInt16), flatType (Std $ IntType UInt16)))
              , ("Builtin.addint32", (CAddOp, flatType (Std $ IntType Int32), flatType (Std $ IntType Int32), flatType (Std $ IntType Int32)))
              , ("Builtin.adduint32", (CAddOp, flatType (Std $ IntType UInt32), flatType (Std $ IntType UInt32), flatType (Std $ IntType UInt32)))
              , ("Builtin.addint64", (CAddOp, flatType (Std $ IntType Int64), flatType (Std $ IntType Int64), flatType (Std $ IntType Int64)))
              , ("Builtin.adduint64", (CAddOp, flatType (Std $ IntType UInt64), flatType (Std $ IntType UInt64), flatType (Std $ IntType UInt64)))

              , ("Builtin.addf32", (CAddOp, flatType (Std $ F32), flatType (Std $ F32), flatType (Std $ F32)))
              , ("Builtin.addf64", (CAddOp, flatType (Std $ F64), flatType (Std $ F64), flatType (Std $ F64)))
              ]

genUnaryOp :: Expr FlatMonoType -> Expr FlatMonoType -> [Expr FlatMonoType] -> ErrorM (Maybe CExpr)
genUnaryOp _ _ _ = return Nothing -- #TODO not implemented yet

genBinaryOp :: Expr FlatMonoType -> Expr FlatMonoType -> [Expr FlatMonoType] -> ErrorM (Maybe CExpr)
genBinaryOp e (Identifier { idName = name }) params = do
    case lookup name cBinOpTable of
        Just (op, t1, t2, t3) -> case params of
            [a, b] -> do
                checkType (exprPos e) (exprNodeData a) t1
                checkType (exprPos e) (exprNodeData b) t2
                checkType (exprPos e) (exprNodeData e) t3
                a' <- genExpr a
                b' <- genExpr b
                return $ Just $ CBinary op a' b' (getNodeInfo (exprPos e))   -- #TODO check types
            _ -> throwFatalError $ text "Compiler Error: Invalid Number of parameters passed to built in binary operator (This should never happen). This is a compiler bug)."
        Nothing -> return Nothing
genBinaryOp _ _ _= return Nothing

genBuiltin :: Expr FlatMonoType -> Expr FlatMonoType -> [Expr FlatMonoType] -> ErrorM (Maybe CExpr)
genBuiltin e f params = fmap msum $ sequence [genUnaryOp e f params, genBinaryOp e f params]


codeGenError :: SourcePos -> String -> ErrorM ()
codeGenError pos s = addError msg
    where
        msg = pPrintPos pos <> colon <+> text "Code Generation error:" $+$  text s


checkType :: SourcePos -> FlatMonoType -> FlatMonoType -> ErrorM ()
checkType pos currentType expectedType = when (currentType /= expectedType) $ codeGenError pos "Compiler Error: Inferred type does not match expected type at code generation. This is a compiler bug."



genExpr :: Expr FlatMonoType -> ErrorM CExpr
genExpr (Literal { exprPos = pos, exprNodeData = monoType, literal = lit } ) = fmap CConst $ case lit of
    StringLiteral s  -> do
        checkType pos monoType $ flatType $ Array (NType ((genericLength s) + 1)) (Std Char8)
        return $ CStrConst (cString s) (getNodeInfo pos)
    CharLiteral c    -> do
        checkType pos monoType $ flatType (Std Char8)
        return $ CCharConst (cChar c) (getNodeInfo pos)
    IntegerLiteral i -> return $ CIntConst (CInteger i DecRepr noFlags) (getNodeInfo pos)   -- #TODO check that type fits, and correct flags (add L if 32 bits LL if 64bits, U if unsigned)
    HexLiteral i     -> return $ CIntConst (CInteger i HexRepr noFlags) (getNodeInfo pos)  
    OctalLiteral i   -> return $ CIntConst (CInteger i OctalRepr noFlags) (getNodeInfo pos)
    FloatLiteral f   -> do
        -- #TODO - We currently don't support double, we should fix this
        checkType pos monoType $ flatType $ Std F32
        return $ CFloatConst (cFloat f) (getNodeInfo pos)
genExpr (Identifier { exprPos = pos, idName = name } ) = return $ CVar (Ident (fmap (\a -> if a == '.' then '_' else a) name) 0 (getNodeInfo pos)) (getNodeInfo pos) -- #TODO we'll need to update the identifierId at some point if we want to run any analysis functions on the output

genExpr e@(Call { exprPos = pos, callFunc = f, callParams = params } ) = do
    maybeBuiltin <- genBuiltin e f params
    case maybeBuiltin of
        Just cCode -> return cCode -- generate code for built in C function, symbol, or operator
        Nothing -> do 
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
index

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
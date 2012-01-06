module Language.TheExperiment.CodeGen where

import Data.Traversable (mapM)
import Prelude hiding (mapM)

import Control.Monad hiding (mapM)

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

-- #TODO possibly put these tables into another file
cUnaryOpTable :: [(String, CUnaryOp)]
cUnaryOpTable = []

cBinOpTable :: [(String, CBinaryOp)]
cBinOpTable = [ ("Builtin.addint8", CAddOp)
              , ("Builtin.adduint8", CAddOp)
              , ("Builtin.addint16", CAddOp)
              , ("Builtin.adduint16", CAddOp)
              , ("Builtin.addint32", CAddOp)
              , ("Builtin.adduint32", CAddOp)
              , ("Builtin.addint64", CAddOp)
              , ("Builtin.adduint64", CAddOp)

              , ("Builtin.addf32", CAddOp)
              , ("Builtin.addf64", CAddOp)
              ]

genUnaryOp :: Expr CodeGenType -> Expr CodeGenType -> [Expr CodeGenType] -> ErrorM (Maybe CExpr)
genUnaryOp _ _ _ = return Nothing -- #TODO not implemented yet

genBinaryOp :: Expr CodeGenType -> Expr CodeGenType -> [Expr CodeGenType] -> ErrorM (Maybe CExpr)
genBinaryOp e (Identifier { idName = name }) params = do
    case lookup name cBinOpTable of
        Just op -> case params of
            [a, b] -> do
                a' <- genExpr a
                b' <- genExpr b
                return $ Just $ CBinary op a' b' (getNodeInfo (exprPos e)) 
            _ -> do
                codeGenError (exprPos e) "Invalid Number of parameters passed to built in binary operator (This should never happen). This is a compiler bug."
                return $ undefined
        Nothing -> return Nothing
genBinaryOp _ _ _= return Nothing

genBuiltin :: Expr CodeGenType -> Expr CodeGenType -> [Expr CodeGenType] -> ErrorM (Maybe CExpr)
genBuiltin e f params = fmap msum $ sequence [genUnaryOp e f params, genBinaryOp e f params]


codeGenError :: SourcePos -> String -> ErrorM ()
codeGenError pos s = addError msg
    where
        msg = pPrintPos pos <> colon <+> text "Code Generation error:" $+$  text s


getIntFlags :: IntType -> Flags CIntFlag
getIntFlags Int8 = noFlags 
getIntFlags UInt8 = setFlag FlagUnsigned noFlags
getIntFlags Int16 = noFlags 
getIntFlags UInt16 = setFlag FlagUnsigned noFlags 
getIntFlags Int32 = setFlag FlagLong noFlags
getIntFlags UInt32 = setFlag FlagUnsigned $ setFlag FlagLong noFlags
getIntFlags Int64 = setFlag FlagLongLong noFlags 
getIntFlags UInt64 = setFlag FlagUnsigned $ setFlag FlagLongLong noFlags 

-- #TODO add documentation about the module names
-- #TODO we'll need to update the identifierId at some point if we want to run any analysis functions on the output
genVar :: SourcePos -> String -> CExpr
genVar pos name = CVar (Ident (fmap (\a -> if a == '.' then '_' else a) name) 0 (getNodeInfo pos)) (getNodeInfo pos)

genExpr :: Expr CodeGenType -> ErrorM CExpr
genExpr (Literal { exprPos = pos, exprNodeData = typ, literal = lit } ) = fmap CConst $ case lit of
        StringLiteral s  -> return $ CStrConst (cString s) (getNodeInfo pos)
        CharLiteral c    -> return $ CCharConst (cChar c) (getNodeInfo pos)
        IntegerLiteral i -> genIntLiteral i DecRepr 
        HexLiteral i     -> genIntLiteral i HexRepr
        OctalLiteral i   -> genIntLiteral i OctalRepr
        FloatLiteral f _ -> return $ CFloatConst (readCFloat f) (getNodeInfo pos)
    where
        genIntLiteral i repr = do
            case typ of
              CStd (IntType t) -> return $ CIntConst (CInteger i repr (getIntFlags t)) (getNodeInfo pos)
              _ -> do 
                codeGenError pos "Compiler Error: Inferred type for an integer literal was not an integer. This is a compiler bug."
                return $ CIntConst (CInteger i repr noFlags) (getNodeInfo pos)


genExpr (Identifier { exprPos = pos, idName = name } ) = return $ genVar pos name

genExpr e@(Call { exprPos = pos, callFunc = f, callParams = params } ) = do
    maybeBuiltin <- genBuiltin e f params
    case maybeBuiltin of
        Just cCode -> return cCode -- return generated code for built in C function, symbol, or operator
        Nothing -> do -- normal function call
            f' <- genExpr f
            params' <- mapM genExpr params
            return $ CCall f' params' (getNodeInfo pos)

genExpr (Member { exprPos = pos, memberExpr = mExpr, memberName = name } ) = do
    mExpr' <- genExpr mExpr
    return $ CMember mExpr' (Ident name 0 (getNodeInfo pos)) False (getNodeInfo pos)


genStatement :: Statement CodeGenType -> ErrorM CStat
genStatement (Assign { stmtPos = pos, assignName = a, assignExpr = b} ) = do
    let a' = genVar pos a
    b' <- genExpr b
    return $ CExpr (Just $ CAssign CAssignOp a' b' (getNodeInfo pos)) (getNodeInfo pos)
genStatement (If { stmtPos = pos, ifCond = cond, ifThen = thenStmt, ifElse = elseStmt } ) = do
    cond' <- genExpr cond
    thenStmt' <- genStatement thenStmt
    elseStmt' <- mapM genStatement elseStmt
    return $ CIf cond' thenStmt' elseStmt' (getNodeInfo pos)


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
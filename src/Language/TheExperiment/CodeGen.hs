module Language.TheExperiment.CodeGen where

{-
Notable features still missing:
    prototype generation
    unary operator generation (index operator too)
    pointer struct member lookup

    structure, union, enum types
    function types
    for loop
    switch statement
    break statement
    declaration initializers
    extern and static storage specifiers (and auto, register, too)
    restricted and inline type qualifiers
    assignment to dereferenced pointers

-}
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
import Language.TheExperiment.CodeGenType

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

genUnaryOp :: Expr GenType -> Expr GenType -> [Expr GenType]
           -> ErrorM (Maybe CExpr)
genUnaryOp _ _ _ = return Nothing -- #TODO not implemented yet

genBinaryOp :: Expr GenType -> Expr GenType -> [Expr GenType] 
            -> ErrorM (Maybe CExpr)
genBinaryOp e (Identifier { idName = name }) params = do
    case lookup name cBinOpTable of
        Just op -> case params of
            [a, b] -> do
                a' <- genExpr a
                b' <- genExpr b
                return $ Just $ CBinary op a' b' (getNodeInfo (exprPos e)) 
            _ -> do
                codeGenError (exprPos e) "Invalid Number of parameters passed\
                    \ to built in binary operator (This should never happen).\
                    \ This is a compiler bug."
                return $ undefined
        Nothing -> return Nothing
genBinaryOp _ _ _= return Nothing

genBuiltin :: Expr GenType -> Expr GenType -> [Expr GenType] -> ErrorM (Maybe CExpr)
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
-- #TODO we'll need to update the identifierId at some point if we want to 
--    run any analysis functions on the output
genVar :: SourcePos -> String -> CExpr
genVar pos name = CVar (genIdent pos (fmap (\a -> if a == '.' then '_' else a) name)) (getNodeInfo pos)


genExpr :: Expr GenType -> ErrorM CExpr
genExpr (Literal { exprPos = pos, exprNodeData = typ, literal = lit } ) = 
        fmap CConst $ case lit of
            StringLiteral s  -> return $ CStrConst (cString s) (getNodeInfo pos)
            CharLiteral c    -> return $ CCharConst (cChar c) (getNodeInfo pos)
            IntegerLiteral i -> genIntLiteral i DecRepr 
            HexLiteral i     -> genIntLiteral i HexRepr
            OctalLiteral i   -> genIntLiteral i OctalRepr
            FloatLiteral f _ -> return $ CFloatConst (readCFloat f) (getNodeInfo pos)
    where
        genIntLiteral i repr = do
            case typ of
              GenType (CStd (IntType t)) _  -> 
                    return $ CIntConst (CInteger i repr (getIntFlags t)) (getNodeInfo pos)
              _ -> do 
                codeGenError pos "Compiler Error: Inferred type for an\
                    \ integer literal was not an integer. This is a compiler\
                    \ bug."
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


genStatement :: Statement GenType -> ErrorM CStat
genStatement (Assign { stmtPos = pos, assignName = a, assignExpr = b} ) = do
    let a' = genVar pos a
    b' <- genExpr b
    return $ CExpr
        (Just $ CAssign CAssignOp a' b' 
            (getNodeInfo pos)) 
        (getNodeInfo pos)
genStatement (If { stmtPos = pos
                 , ifCond = cond
                 , ifThen = thenStmt
                 , ifElse = elseStmt } ) = do
    cond' <- genExpr cond
    thenStmt' <- genStatement thenStmt
    elseStmt' <- mapM genStatement elseStmt
    return $ CIf cond' thenStmt' elseStmt' (getNodeInfo pos)
genStatement (While { stmtPos = pos
                    , whileCond = cond
                    , whileBody = body } ) = do
    cond' <- genExpr cond
    body' <- genStatement body
    return $ CWhile cond' body' False (getNodeInfo pos)
genStatement (ExprStmt { stmtPos = pos
                       , stmtExpr = a }) = do
    a' <- genExpr a
    return $ CExpr (Just $ a') (getNodeInfo pos)
genStatement (Return { stmtPos = pos
                     , returnExpr = a }) = do
    a' <- genExpr a
    return $ CReturn (Just $ a') (getNodeInfo pos)
genStatement (Block { stmtPos = pos
                    , blockBody = (varDefs, stmts) }) = do
    -- #TODO is there a nice way to restrict this to only be vardefs?
    -- #TODO  we also only currently support variable declarations without initializers
    let varDefs' = [genVarDecl vPos name t | (TopVarDef { topStmtPos = vPos
                                                        , topStmtNodeData = t
                                                        , varDef = VarDef { varName = name } } ) <- varDefs]
    stmts' <- mapM genStatement stmts
    return $ CCompound [] (fmap CBlockDecl varDefs' ++ fmap CBlockStmt stmts') (getNodeInfo pos)

genVarDef :: VarDef GenType -> CDecl
genVarDef (VarDef { varDefPos = pos, varDefNodeData = t, varName = name } ) = genVarDecl pos name t


genModule :: Module GenType -> ErrorM CTranslUnit
genModule (Module pos topLevels) = do
    topLevels' <- mapM genTopLevel topLevels
    return $ CTranslUnit topLevels' (getNodeInfo pos)

genTopLevel :: TopLevelStmt GenType -> ErrorM CExtDecl
-- #TODO we currently do not handle var defs with initializers
genTopLevel (TopVarDef { varDef = def } ) = return $ CDeclExt $ genVarDef def
genTopLevel (TypeDef { topStmtPos = pos
                     , topStmtNodeData = t
                     , typeDefName = name } ) 
            = return $ CDeclExt $ genTypeDef pos name t
genTopLevel (FuncDef { topStmtPos = pos
                     , topStmtNodeData = GenType retType _
                     , funcName = name
                     , funcParams = params
                     , funcStmt = stmt } ) 
            = liftM CFDefExt $ genFuncDef pos name params retType stmt


genTypeDef :: SourcePos -> String -> GenType -> CDecl
genTypeDef pos name t = genDecl pos name t [CStorageSpec (CTypedef (getNodeInfo pos))]

genFuncDef :: SourcePos -> String -> [VarDef GenType] -> GenBasicType -> Statement GenType -> ErrorM CFunDef
genFuncDef pos name params retType stmt = do
    let retType' = genTypeSpec pos retType
    let params' = fmap genVarDef params
    let funcDeclr = CFunDeclr (Right (params', False)) [] (getNodeInfo pos)
    let declr = CDeclr (Just (genIdent pos name)) [funcDeclr] Nothing [] (getNodeInfo pos)
    stmt' <- genStatement stmt
    return $ CFunDef [CTypeSpec retType'] declr [] stmt' (getNodeInfo pos)



genIdent :: SourcePos -> String -> Ident
genIdent pos name = Ident name 0 (getNodeInfo pos)

genDecl :: SourcePos -> String -> GenType -> [CDeclSpec] -> CDecl
genDecl pos name (GenType t derivDecls) spec = CDecl 
    (spec ++ [CTypeSpec $ genTypeSpec pos t])
    [(Just $ genDeclr pos name (fmap (genDerivDecl pos) derivDecls), Nothing, Nothing)]
    (getNodeInfo pos)

genDeclr :: SourcePos -> String -> [CDerivedDeclr] -> CDeclr
genDeclr pos name derivDeclr = CDeclr (Just $ genIdent pos name) derivDeclr Nothing [] (getNodeInfo pos)

genVarDecl :: SourcePos -> String -> GenType -> CDecl
genVarDecl pos name t = genDecl pos name t []

genDerivDecl :: SourcePos -> GenTypeDerivDecl -> CDerivedDeclr
genDerivDecl pos (CArrayDecl quals i) = 
    CArrDeclr 
        (fmap (genTypeQual pos) quals) 
        (CArrSize False $ CConst $ CIntConst 
            (CInteger i DecRepr noFlags) 
            (getNodeInfo pos)) 
        (getNodeInfo pos)
genDerivDecl pos (CPointerDecl quals) = CPtrDeclr (fmap (genTypeQual pos) quals) (getNodeInfo pos)

genTypeQual :: SourcePos -> GenTypeQualifier -> CTypeQual
genTypeQual pos CImmutable = CConstQual (getNodeInfo pos)
genTypeQual pos CVolatile = CVolatQual (getNodeInfo pos)


genTypeSpec :: SourcePos -> GenBasicType -> CTypeSpec
genTypeSpec pos (CTypeName name) = CTypeDef (genIdent pos name) (getNodeInfo pos)
genTypeSpec pos (CStd stdType) = genStdTypeSpec pos stdType

genStdTypeSpec :: SourcePos -> StdType -> CTypeSpec
genStdTypeSpec pos Void = CVoidType (getNodeInfo pos)
genStdTypeSpec pos Char8 = CCharType (getNodeInfo pos)
genStdTypeSpec pos (IntType t) = genIntTypeSpec pos t
genStdTypeSpec pos SBool = CBoolType (getNodeInfo pos)
genStdTypeSpec pos F32 = CFloatType (getNodeInfo pos)
genStdTypeSpec pos F64 = CDoubleType (getNodeInfo pos)

genIntTypeSpec :: SourcePos -> IntType -> CTypeSpec
genIntTypeSpec pos Int8 = CTypeDef (genIdent pos "int8_t") (getNodeInfo pos)
genIntTypeSpec pos UInt8 = CTypeDef (genIdent pos "uint8_t") (getNodeInfo pos)
genIntTypeSpec pos Int16 = CTypeDef (genIdent pos "int16_t") (getNodeInfo pos)
genIntTypeSpec pos UInt16 = CTypeDef (genIdent pos "uint16_t") (getNodeInfo pos)
genIntTypeSpec pos Int32 = CTypeDef (genIdent pos "int32_t") (getNodeInfo pos)
genIntTypeSpec pos UInt32 = CTypeDef (genIdent pos "uint32_t") (getNodeInfo pos)
genIntTypeSpec pos Int64 = CTypeDef (genIdent pos "int64_t") (getNodeInfo pos)
genIntTypeSpec pos UInt64 = CTypeDef (genIdent pos "uint64_t") (getNodeInfo pos)


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
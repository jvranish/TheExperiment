module Language.TheExperiment.CodeGen where

{-
Notable features still missing:
    casts
    prototype generation
    index operator
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

import Control.Monad

import Text.Parsec.Pos

import Language.C.Syntax
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position

import Language.TheExperiment.Type
import Language.TheExperiment.AST
import Language.TheExperiment.Builtin
import Language.TheExperiment.CodeGenType

getNodeInfo :: SourcePos -> NodeInfo
getNodeInfo pos = OnlyPos cPos (cPos, 0)
    where
        cPos = position 0 (sourceName pos) (sourceLine pos) (sourceColumn pos) 

genUnaryOp :: Expr GenType -> Expr GenType -> [Expr GenType] ->  Maybe CExpr
genUnaryOp e (Identifier { idName = name }) params = do
    case lookup name cUnaryOpTable of
        Just op -> case params of
            [a] -> Just $ 
                CUnary op (genExpr a) (getNodeInfo (exprPos e)) 
            _      -> Nothing
                -- #TODO maybe move this check into an earlier step
                --   Invalid Number of parameters passed
                --  to built in binary operator (This should never happen)
        Nothing -> Nothing
genUnaryOp _ _ _ = Nothing

genBinaryOp :: Expr GenType -> Expr GenType -> [Expr GenType] -> Maybe CExpr
genBinaryOp e (Identifier { idName = name }) params = do
    case lookup name cBinOpTable of
        Just op -> case params of
            [a, b] -> Just $ 
                CBinary op (genExpr a) (genExpr b) (getNodeInfo (exprPos e)) 
            _      -> Nothing
                -- #TODO maybe move this check into an earlier step
                --   Invalid Number of parameters passed
                --  to built in binary operator (This should never happen)
        Nothing -> Nothing
genBinaryOp _ _ _= Nothing

genBuiltin :: Expr GenType -> Expr GenType -> [Expr GenType] -> Maybe CExpr
genBuiltin e f params = msum $ [genUnaryOp e f params, genBinaryOp e f params]


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
genVar pos name = 
    CVar 
        (genIdent pos (fmap (\a -> if a == '.' then '_' else a) name)) 
        (getNodeInfo pos)

genIntLiteral :: SourcePos -> GenType -> Integer -> CIntRepr -> CConst
genIntLiteral pos typ i repr = case typ of
      GenType (CStd (IntType t)) _  -> 
            CIntConst (CInteger i repr (getIntFlags t)) (getNodeInfo pos)
     -- We should never hit this case
      _ -> CIntConst (CInteger i repr noFlags) (getNodeInfo pos)

genExpr :: Expr GenType -> CExpr
genExpr (Literal { exprPos      = pos
                 , exprNodeData = typ
                 , literal = lit } ) = CConst $ case lit of
            StringLiteral s  -> CStrConst (cString s) (getNodeInfo pos)
            CharLiteral c    -> CCharConst (cChar c) (getNodeInfo pos)
            IntegerLiteral i -> genIntLiteral pos typ i DecRepr 
            HexLiteral i     -> genIntLiteral pos typ i HexRepr
            OctalLiteral i   -> genIntLiteral pos typ i OctalRepr
            FloatLiteral f _ -> CFloatConst (readCFloat f) (getNodeInfo pos)
genExpr (Identifier { exprPos = pos
                    , idName = name } ) = genVar pos name
genExpr e@(Call { exprPos    = pos
                , callFunc   = f
                , callParams = params } ) = 
    case genBuiltin e f params of
        Just cCode -> cCode -- return generated code for built in C function,
                            -- symbol, or operator
        Nothing -> CCall (genExpr f) (fmap genExpr params) (getNodeInfo pos)

-- #TODO decide how to handle pointer member references
genExpr (Member { exprPos    = pos
                , memberExpr = mExpr
                , memberName = name } ) = 
    CMember 
        (genExpr mExpr)
        (Ident name 0 (getNodeInfo pos))
        False
        (getNodeInfo pos)

genStatement :: Statement GenType -> CStat
genStatement (Assign { stmtPos    = pos
                     , assignName = a
                     , assignExpr = b} ) = 
    CExpr
        (Just $ 
            CAssign CAssignOp (genVar pos a) (genExpr b) (getNodeInfo pos))
        (getNodeInfo pos)

genStatement (If { stmtPos = pos
                 , ifCond = cond
                 , ifThen = thenStmt
                 , ifElse = elseStmt } ) = 
    CIf 
        (genExpr cond)
        (genStatement thenStmt)
        (fmap genStatement elseStmt)
        (getNodeInfo pos)
genStatement (While { stmtPos = pos
                    , whileCond = cond
                    , whileBody = body } ) = 
    CWhile (genExpr cond) (genStatement body) False (getNodeInfo pos)
genStatement (ExprStmt { stmtPos = pos
                       , stmtExpr = a }) =
    CExpr (Just $ genExpr a) (getNodeInfo pos)
genStatement (Return { stmtPos = pos
                     , returnExpr = a }) =
    CReturn (Just $ genExpr a) (getNodeInfo pos)
genStatement (Block { stmtPos = pos
                    , blockBody = (varDefs, stmts) }) =
        CCompound 
            [] 
            (varDefs' ++ stmts')
            (getNodeInfo pos)
    -- #TODO is there a nice way to restrict this to only be vardefs?
    -- #TODO we also only currently support variable declarations without 
    --    initializers
    where
        stmts'   = fmap (CBlockStmt . genStatement) stmts
        varDefs' = [CBlockDecl $ genVarDecl vPos name t | 
                        (TopVarDef { topStmtPos = vPos
                                   , topStmtNodeData = t
                                   , varDef = VarDef { varName = name } } )
                        <- varDefs]

genVarDef :: VarDef GenType -> CDecl
genVarDef (VarDef { varDefPos = pos
                  , varDefNodeData = t
                  , varName = name } ) = genVarDecl pos name t


genModule :: Module GenType -> CTranslUnit
genModule (Module pos topLevels) = 
    CTranslUnit (fmap genTopLevel topLevels) (getNodeInfo pos)

genTopLevel :: TopLevelStmt GenType -> CExtDecl
-- #TODO we currently do not handle var defs with initializers
genTopLevel (TopVarDef { varDef = def } ) = CDeclExt $ genVarDef def
genTopLevel (TypeDef { topStmtPos = pos
                     , topStmtNodeData = t
                     , typeDefName = name } ) 
            = CDeclExt $ genTypeDef pos name t
genTopLevel (FuncDef { topStmtPos = pos
                     , topStmtNodeData = GenType retType _
                     , funcName = name
                     , funcParams = params
                     , funcStmt = stmt } ) 
            = CFDefExt $ genFuncDef pos name params retType stmt


genTypeDef :: SourcePos -> String -> GenType -> CDecl
genTypeDef pos name t = 
    genDecl pos name t [CStorageSpec (CTypedef (getNodeInfo pos))]

genFuncDef :: SourcePos -> String -> [VarDef GenType] -> GenBasicType
           -> Statement GenType -> CFunDef
genFuncDef pos name params retType stmt =
    CFunDef [CTypeSpec retType'] declr [] stmt' (getNodeInfo pos)
    where
        retType'  = genTypeSpec pos retType
        params'   = fmap genVarDef params
        funcDeclr = CFunDeclr (Right (params', False)) [] (getNodeInfo pos)
        declr     = CDeclr 
                        (Just (genIdent pos name))
                        [funcDeclr]
                        Nothing
                        []
                        (getNodeInfo pos)
        stmt'     = genStatement stmt



genIdent :: SourcePos -> String -> Ident
genIdent pos name = Ident name 0 (getNodeInfo pos)

genDecl :: SourcePos -> String -> GenType -> [CDeclSpec] -> CDecl
genDecl pos name (GenType t derivDecls) spec = CDecl 
    (spec ++ [CTypeSpec $ genTypeSpec pos t])
    [ (Just $ genDeclr pos name (fmap (genDerivDecl pos) derivDecls)
      , Nothing
      , Nothing ) ]
    (getNodeInfo pos)

genDeclr :: SourcePos -> String -> [CDerivedDeclr] -> CDeclr
genDeclr pos name derivDeclr = 
    CDeclr (Just $ genIdent pos name) derivDeclr Nothing [] (getNodeInfo pos)

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
genDerivDecl pos (CPointerDecl quals) = 
    CPtrDeclr (fmap (genTypeQual pos) quals) (getNodeInfo pos)

genTypeQual :: SourcePos -> GenTypeQualifier -> CTypeQual
genTypeQual pos CImmutable = CConstQual (getNodeInfo pos)
genTypeQual pos CVolatile = CVolatQual (getNodeInfo pos)


genTypeSpec :: SourcePos -> GenBasicType -> CTypeSpec
genTypeSpec pos (CTypeName name) = 
    CTypeDef (genIdent pos name) (getNodeInfo pos)
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
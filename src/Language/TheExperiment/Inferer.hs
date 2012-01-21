module Language.TheExperiment.Inferrer where

import Language.TheExperiment.AST

import qualified Data.Map as Map

-- TODO list:
{-
function to tag each node with environment
copy type (with a functor)
specializer
overload resolution
stmt inferrer
finish Inferrer monad

add ability to catch error from unify monad to improve error messages

-}

data Environment = Environment { valueEnv :: Map.Map String (TopLevelStmt NodeData)
                                 -- , typeEnv  :: Map.Map String (TypeDef v)
                                 }
    deriving (Show, Eq, Ord)

data TypeRef = TypeRef
    deriving (Show, Eq, Ord)

data NodeData = NodeData { nodeEnv :: Environment 
                         , typeRef :: TypeRef
                         }
    deriving (Show, Eq, Ord)

instance NodeType TypeRef where
instance NodeType NodeData where

localEnv :: Environment -> [(String, TopLevelStmt NodeData)] -> Environment
localEnv (Environment a) xs = Environment $ Map.union (Map.fromList xs) a

localDefs :: Environment -> [TopLevelStmt NodeData] -> Environment
localDefs env defs = localEnv env $ 
    fmap (\stmt -> (getTopLevelName stmt, stmt)) defs

getTopLevelName :: TopLevelStmt NodeData -> String
getTopLevelName (TopVarDef { varDef      = varDef }) = varName varDef
getTopLevelName (FuncDef   { funcName    = name   }) = name
getTopLevelName (TypeDef   { typeDefName = name   }) = name

applyScope :: (Functor f) => Environment -> f TypeRef -> f NodeData
applyScope env a = fmap (\x -> NodeData env x) a

scopeStatement :: Environment -> Statement TypeRef -> Statement NodeData
scopeStatement env a@(Assign {})   = applyScope env a
scopeStatement env a@(If {})       = applyScope env a
scopeStatement env a@(While {})    = applyScope env a
scopeStatement env a@(ExprStmt {}) = applyScope env a
scopeStatement env a@(Return {})   = applyScope env a
scopeStatement env (Block pos t (tlStmts, stmts)) = let
    env'     = localDefs env tlStmts'
    tlStmts' = fmap (scopeTopLevelStmt env') tlStmts
    stmts'   = fmap (scopeStatement env') stmts
    in Block pos (NodeData env' t) (tlStmts', stmts')

scopeTopLevelStmt :: Environment -> TopLevelStmt TypeRef 
                  -> TopLevelStmt NodeData
scopeTopLevelStmt env (FuncDef pos t funcName params stmt) = let        
    params' = fmap (applyScope env) params
    stmt'  = scopeStatement env' stmt
    env'    = localDefs env $ fmap topVar params'
    topVar a@(VarDef pos nodeData _) = TopVarDef pos nodeData a
    in FuncDef pos (NodeData env' t) funcName params' stmt'
scopeTopLevelStmt env a@(TypeDef {})   = applyScope env a 
scopeTopLevelStmt env a@(TopVarDef {}) = applyScope env a 


scopeModule :: Environment -> Module TypeRef -> Module NodeData
scopeModule env (Module pos stmts) = let 
    -- we can do this because laziness is awesome
    env' = localDefs env stmts'
    stmts' = fmap (scopeTopLevelStmt env') stmts
    in Module pos stmts'
      
{-
inferExpr :: Expr -> Inferrer TypeRef


data Type a = TypeName String
            | Std StdType
            | Pointer a
            | Array a a -- first parameter should only be a type variable or 
                        -- an NType (type level number), eventually this would 
                        -- be moved to the std libs, and have a constraint on
                        -- that parameter
            | NType Integer


-- reference expr, expected, inferred
unify :: Expr TypeRef -> TypeRef -> TypeRef -> Inferrer TypeRef
unify expr expected inferred = do
        expected' <- readType expected
        inferred' <- readType inferred
        unify' expected' inferred'
    where
        unify' (TypeName a)  (TypeName b) | a == b = success
        unify' (Std a)       (Std b)      | a == b = success
        unify' (NType a)     (NType b)    | a == b = success
        unify' (Pointer a)   (Pointer b) = do
            aUb <- unify a b
            newType $ Pointer aUb
        unify' (Array a1 a2) (Array b1 b2) = do
            a1Ub1 <- unify a1 b1
            a2Ub2 <- unify a2 b2
            newType $ Array a1Ub1 a2Ub2
        unify' (Func aParams aRet) (Func bParams bRet) = do
            params <- unifyList aParams bParams
            ret <- unify aRef bRet
            newType $ Func params ret
        unify' (Var aName (Fields aFields) aOverloads) 
               (Var bName (Fields bFields) bOverloads) = do
            fields <- unionWithM (unify expr) aFields bFields
            overloads <- mergeOverloads aOverloads bOverloads
            newType $ Var (mplus aName bName) (Fields fields) overloads
        unify' (Var mName fields overloads) _ = 
            unifyVar mName fields overloads inferred
        unify' _ (Var mName fields overloads) = 
            unifyVar mName fields overloads expected
        unify' _               _           = failure

        unifyVar name fields NotOverloaded t | fields == noRecFields = return t
                                             | otherwise = do
      always make sure t has all the required fields (don't worry if none)
      if no overloads, replace var with type
         otherwise = 

        unifyVar mName fields overloads t = do
            if non empty recfields -> make sure struct has fields
            if overloads
                unify t with expected, reduce etc...
            

        mergeOverloads (Overloads a as) (Overloads b bs) = do
            u <- unify a b
            us <- intersectByM typeEq as bs
            when (us == []) $ error "overload fail"
            return $ Overloads u us
        mergeOverloads NotOverloaded b = return b
        mergeOverloads a NotOverloaded = return a

        unifyList [] [] = return []
        unifyList (x:xs) (y:ys) = do
            u <- unify expr x y
            us <- unifyList xs
            return (u:us)
        unifyList _ _ = error "unify failure (probably function param mismatch"

        success = do
            subsType inferred expected
            return $ expected
        failure = error "unify failure"



inferExpr :: Map.Map String (Statement TypeRef) 
          -> Map.Map String (TypeDef TypeRef) -> Expr TypeRef -> Inferrer TypeRef
inferExpr env typeEnv expr = 
    case expr of 
        Call { callFunc   = f
             , callParams = params } -> do

            f' <- inferExpr env typeEnv f
            params' <- mapM (inferExpr env typeEnv) params
            retType <- newTypeVar
            inferredF <- newType $ Func params' retType
            unify expr f' inferredF
        Identifier { idName = name } -> do

        Literal { literal = lit } -> do

        Member { memberExpr = mExpr
               , memberName = name } -> do
            
inferStmt :: Map.Map String (Statement TypeRef) 
          -> Map.Map String (TypeDef TypeRef) -> Statement TypeRef 
          -> Inferrer TypeRef
inferStmt env typeEnv stmt = undefined

-}

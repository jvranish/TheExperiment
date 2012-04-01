{-#Language GeneralizedNewtypeDeriving
          , StandaloneDeriving
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          , FlexibleContexts
          , UndecidableInstances
          #-}
module Language.TheExperiment.Inferrer.Inferrer where

import Text.Parsec.Pos

import Control.Monad hiding (msum, mapM, mapM_, forM)
import Control.Monad.Trans.State
import Control.Monad.Trans

import Data.Maybe
import Data.Monoid
import Data.Either
import Data.Foldable
import Data.Traversable

import Data.Char
import qualified Data.List as List

import Text.PrettyPrint

import qualified Data.Map as Map

import Language.TheExperiment.Misc
import Language.TheExperiment.Parser.AST.Statement
import Language.TheExperiment.Parser.AST.Expression
import Language.TheExperiment.Parser.AST.Module

import Language.TheExperiment.Pretty.Expression

import qualified Language.TheExperiment.Parser.AST.Type as ParserAST

import Language.TheExperiment.Inferrer.Type
import Language.TheExperiment.Inferrer.InferrerM
import Language.TheExperiment.Inferrer.Unify
import Language.TheExperiment.Inferrer.Scope
import Language.TheExperiment.Inferrer.Pretty

import Language.TheExperiment.CodeGen.Type

import qualified Control.Monad.GraphT as GraphT
import qualified Control.Monad.ErrorM as ErrorM

import Prelude hiding (concat, concatMap, mapM, mapM_)

import Debug.Trace

prepareAST :: (Traversable f) => f a -> StateT Int Inferrer (f (TypeRef, NodeId))
prepareAST t = forM t $ \_ -> do
  ref <- lift $ newType $ Var Nothing []
  n <- getAndModifyT (+1)
  return (ref, n)


runInferrer :: Inferrer a -> ErrorM.ErrorM a
runInferrer (Inferrer m) = GraphT.runGraphT (evalStateT m (Map.empty))

infer :: Module a -> ErrorM.ErrorM (Module GenType)
infer m@(Module pos _) = runInferrer $ do
  m' <- evalStateT (prepareAST m) 0
  let m'' = scopeModule mempty m'
  inferModule m''
  checkTypes m''
  topStmts <- fmap (fmap snd) $ execStateT (specializeModule m'') []
  mapM convertNodeData (Module pos topStmts)




convertTypeSig :: ParserAST.TypeSignature -> Inferrer TypeRef
convertTypeSig (ParserAST.TypeSignature constraints t) = flip evalStateT Map.empty $ do
  table <- mapM makeConstraint constraints
  put $ Map.fromList table
  convertParsedType t

makeConstraint :: ParserAST.TypeConstraint
               -> StateT (Map.Map String TypeRef) Inferrer (String, TypeRef)
makeConstraint (ParserAST.TypeConstraint name constraints) = do
  constraintRefs <- mapM convertParsedType constraints
  var <- lift $ newType $ Var (Just name) constraintRefs
  return (name, var)

convertParsedType :: ParserAST.ParsedType
                  -> StateT (Map.Map String TypeRef) Inferrer TypeRef
convertParsedType (ParserAST.TypeName { ParserAST.typeName = name }) 
-- #TODO add more stuff here (maybe use Read?)
  | name == "UInt32" = lift $ newType $ Std $ IntType UInt32
  | name == "Bool" = lift $ newType $ Std $ SBool
  | otherwise = lift $ newType $ TypeName name
convertParsedType (ParserAST.TypeVariable { ParserAST.typeVarName = name }) = do
  StateT $ \table -> do
    case Map.lookup name table of
      Just ref -> return (ref, table)
      Nothing  -> do
        ref <- newType $ Var (Just name) []
        return (ref, Map.insert name ref table)
-- #TODO
-- convertParsedType (TypeCall {  })  unsupported for the moment
convertParsedType (ParserAST.FunctionType { ParserAST.typeArgs = args 
                                    , ParserAST.returnType  = ret }) = do
  argRefs <- mapM convertParsedType args
  retRef <- convertParsedType ret
  lift $ newType $ Func argRefs retRef

{-
copySigType :: Definition NodeData -> Inferrer TypeRef
copySigType (TopVarDef { sig = Just typeSig }) = convertTypeSig typeSig
copySigType (FunctionDef { sig = Just typeSig }) = convertTypeSig typeSig
copySigType (ForeignDef { sig = Just typeSig }) = convertTypeSig typeSig
copySigType _ = newType $ Var Nothing []
-}

lookupType :: SourcePos -> Env -> String -> Inferrer TypeRef
lookupType pos (Env env _) name = do 
  case Map.lookup name env of
    Nothing -> do
      addError $ prettyError pos ("Not in scope: " ++ name) empty
      newType $ Var Nothing []
    Just topStmt -> inferDefinition topStmt 

lookupRetType :: Statement NodeData -> Inferrer TypeRef
lookupRetType stmt = case enclosingFunction $ nodeEnv $ stmtNodeData stmt of
  Just (FunctionDef { functionRet = n }) -> return $ typeRef n
  _ -> do
    addError $ prettyError (stmtPos stmt) "No Enclosing function, this should never happen " empty
    newType $ Var Nothing []


inferExpr :: Expr NodeData -> Inferrer TypeRef
inferExpr expr = let
    t = typeRef $ exprNodeData expr
    errLoc = (UnifyErrorLoc (exprPos expr) (prettyExpression expr))
    in
    case expr of 
        Call { callFunc   = f
             , callParams = params } -> do

            f' <- inferExpr f
            params' <- mapM (inferExpr) params
            retType <- newType $ Var Nothing []
            inferredF <- newType $ Func params' retType
            _ <- unify errLoc f' inferredF
            return retType
        Identifier { idName = name } -> do
            a <- lookupType (exprPos expr) (nodeEnv $ exprNodeData expr) name -- do I put the inferrer in the lookup?
            unify errLoc t a
        Literal { literal = lit } -> do
            case lit of
                StringLiteral s -> do
                    elementType <- newType $ Std Char8
                    sizeType <- newType $ NType $ List.genericLength s
                    newType $ Array sizeType elementType
                CharLiteral _ -> newType $ Std Char8
                FloatLiteral _ _ -> do
                    doubleType <- newType $ Std F64
                    floatType <- newType $ Std F32
                    newType $ Var Nothing [doubleType, floatType]
                IntegerLiteral n -> inferInteger n
                BinLiteral n -> inferInteger n
                HexLiteral n -> inferInteger n
                OctalLiteral n -> inferInteger n
                
        -- Member { memberExpr = mExpr
        --       , memberName = name } -> do
  where
    -- #TODO think about how to integrate this into the standard library somehow
    inferInteger n = do
      allowableTypes <- mapM newType $ concat $ fmap validInts [minBound ..]
      case allowableTypes of
        [] -> do
          addError $ prettyError (exprPos expr)
            "Integer is too big to fit in C type" empty 
          newType $ Var Nothing []
        _ -> newType $ Var Nothing allowableTypes
      where
          validInts t | isValidInt t n = [Std $ IntType t] 
                      | otherwise      = []

{-}
boolType :: Inferrer TypeRef
boolType = newType $ Std SBool
voidType :: Inferrer TypeRef
voidType = newType $ Std Void
-}

inferStmt :: Statement NodeData -> Inferrer ()
inferStmt stmt = do
    let lookupName = lookupType (stmtPos stmt) (nodeEnv $ stmtNodeData stmt)
    let t = typeRef $ stmtNodeData stmt
    boolType <- newType $ Std SBool
    _ <- case stmt of
        Assign { assignName = name
               , assignExpr = expr } -> do
            a <- lookupName name
            b <- inferExpr expr
            _ <- unify (UnifyErrorLoc (exprPos expr) (prettyExpression expr)) a b
            return ()
        If { ifCond = condExpr
           , ifThen = thenStmt
           , ifElse = elseStmtM } -> do
            condType <- inferExpr condExpr
            _ <- unify (UnifyErrorLoc (exprPos condExpr) (prettyExpression condExpr)) boolType condType
            inferStmt thenStmt
            mapM_ (inferStmt) elseStmtM
        While { whileCond = condExpr
              , whileBody = bodyStmt } -> do
            condType <- inferExpr condExpr
            _ <- unify (UnifyErrorLoc (exprPos condExpr) (prettyExpression condExpr)) boolType condType
            inferStmt bodyStmt
            return ()
        CallStmt { stmtExpr = expr } -> do
            _ <- inferExpr expr
            return ()
        Return { returnExpr = expr } -> do
            a <- lookupRetType stmt
            b <- inferExpr expr
            _ <- unify (UnifyErrorLoc (exprPos expr) (prettyExpression expr)) a b
            return ()
        Block { blockBody = body} -> do
            mapM_ inferDefOrStatement body
    writeType t (Std Void) -- hmmmm

inferDefOrStatement :: DefOrStatement NodeData -> Inferrer ()
inferDefOrStatement (Def def) = inferDefinition def >> return ()
inferDefOrStatement (Stmt stmt) = inferStmt stmt


getSigType def = case typeSig $ defnNodeData def of
  Just sig -> convertTypeSig sig
  Nothing -> newType $ Var Nothing []

-- #TODO replace topStmt with 'def' or something
inferDefinition :: Definition NodeData -> Inferrer TypeRef
inferDefinition def = do
  let t = typeRef $ defnNodeData def
  let nId = nodeId $ defnNodeData def
  status <- getInferStatus nId
  case status of
    -- If we are currently inferring this same toplevel this means that
    --  we've run into it again on a recursive call. In this case we
    --  want to unify with it monomorphically. (We can't copy it since we
    --  aren't finished inferring the type yet!) 
    Just Inferring -> return t
    -- If we've inferred this type already, then we just have to decide if
    --  we should return a copy of the type (for polymorphic references)
    --  or the type itself (for monomorphic references)
    Just Inferred -> pickPolyMono t
    Nothing -> do
      setInferStatus nId Inferring
      -- inject signature type here before any actual type has been inferred
      -- if there is no signature, this will have no effect
      sigType <- getSigType def
      tStr <- liftM render $ prettyTypeRef t
      let errorStr = (show $ typeSig $ defnNodeData def) ++ " " ++ tStr

      _ <- unify (error ("signature unify error (impossible): " ++ errorStr ++ "\n")) sigType t
      inferredType <- inferDefinitionMemo def
      setInferStatus nId Inferred
      pickPolyMono inferredType
  where
    pickPolyMono t = do
      case def of
        -- For functions, copy the type. The caller will unify with the 
        --  copied type which will keep the function type polymorphic
        FunctionDef { } -> copyType t
        ForeignDef { } -> copyType t
        -- Everything else we want monomorphic
        _ -> return t

inferDefinitionMemo :: Definition NodeData -> Inferrer TypeRef
inferDefinitionMemo def = do
    let t = typeRef $ defnNodeData def
    case def of
        VariableDef { } -> do
            -- this doesn't do anything currently, but will when there are
            --  initialization expressions 
            return ()
        FunctionDef { functionBlock   = stmt
                , functionName   = name
                , functionParams = params
                , functionRet    = ret }-> do
            inferStmt stmt
            tStr <- showType t
            let paramTypes = trace (name ++ ": " ++ tStr ++ "\n") $ fmap (typeRef . varDefNodeData) params
            --retType <- lookupType (stmtPos stmt) (nodeEnv $ stmtNodeData stmt) returnId 
            let retType = typeRef $ ret
            --funcType <- newType $ 
            --retType <- newType $ Var Nothing NotOverloaded
            -- writeType t $ Func paramTypes retType --funcType
            q <- newType $ Func paramTypes retType --funcType
            sigStr <- showType q
            
            let errorString = sigStr ++ "\n" ++ tStr ++ "\n"
            _ <- unify (error $ "lv inferrer\n" ++ errorString) t q
            return ()
        _ -> return ()
        -- hmmmm, what should I do here?
        -- TypeDef { } -> return ()
    return t

inferModule :: Module NodeData -> Inferrer ()
inferModule (Module _ defs) = mapM_ inferDefinition defs

getFuncName :: Definition NodeData -> Maybe (String, NodeData)
getFuncName (FunctionDef { functionName        = name
                         , defnNodeData = nodeData }) = Just (name, nodeData)
getFuncName _ = Nothing




convertNodeData :: NodeData -> Inferrer GenType
convertNodeData (NodeData { typeRef = ref}) = convertTypes [] ref

convertTypes :: [GenTypeDerivDecl] -> TypeRef -> Inferrer GenType
convertTypes decl ref = do
    t <- readType ref
    case t of
      TypeName name     -> return $ GenType (CTypeName name) decl
      Std std           -> return $ GenType (CStd std) decl 
      -- #TODO check the order here (I think it might be backwards)
      Pointer a         -> convertTypes ((CPointerDecl []):decl) a
      Array n a -> do
        n' <- readType n
        case n' of
          (NType n'') -> convertTypes ((CArrayDecl [] n''):decl) a
          _           -> convertError "bad array length type"
      Func _ _  -> return $ GenType (CStd (IntType Int8)) decl -- convertError "we do not support function types yet"
      Var _ _   -> return $ GenType (CStd (IntType Int8)) decl -- convertError "Something is broken. Var in output"
  where
    convertError msg = do
      addError $ text msg
      return $ GenType (CTypeName "error") []


specializeModule :: Module NodeData -> StateT [(NodeId, Definition NodeData)] Inferrer ()
specializeModule (Module _ topStmts) = 
  mapM_ (uncurry specialize) (catMaybes $ fmap getFuncName topStmts) 

-- #TODO foreign functions cannot be specialized.... so we should 
--   add some protections around them or something



specialize :: String -> NodeData 
           -> StateT [(NodeId, Definition NodeData)] Inferrer ()
specialize name (NodeData { nodeEnv = Env env _
                          , typeRef = t }) = do
    a <- lookupDef
    case a of
      Just f@(FunctionDef {}) -> do
        let lense n = (typeRef n, \t' -> n {typeRef = t'})
        f' <- lift $ copyWithNewTypes lense f
        t' <- lift $ copyType t
        -- This will cause a horrible crash if there is a unify failure
        -- #TODO refactor how I do error checking in unify
        -- #TODO remove all these "errorString" things when that happens too
        sigStr <- lift $ showType (typeRef $ defnNodeData f')
        tStr <- lift $ showType t
        let errorString = sigStr ++ "\n" ++ tStr ++ "\n"
        _ <- lift $ unify (error $ "error in specialize of " ++ name ++ ":\n" ++ errorString) t' (typeRef $ defnNodeData f')
        let names = getNames f'
        modify $ ((nodeId $ defnNodeData f', f') :)
        mapM_ (uncurry specialize) names

      _ -> return ()
  where
{-
      lookup name, 
      get nodeid of the looked up item
        look up nodeid in the already specialized list
           check the list of results against the requested type in nodeData
           if there is a match, then abort (this has already been specialized)
-}
    lookupDef = do
      case Map.lookup name env of
        Nothing -> return Nothing -- This is an error, but should get
                                  -- caught at the infer stage
        Just f -> do
          specialized <- get 
          let potentials = lookups (nodeId $ defnNodeData f) specialized
          a <- lift $ anyM (typeEq t) 
                           (fmap (typeRef . defnNodeData) potentials)
          case a of
            True -> return Nothing
            False -> return $ Just f



getNames :: Definition NodeData -> [(String, NodeData)]
getNames (VariableDef {}) = []
getNames (FunctionDef { functionBlock = stmt }) = getStmtNames stmt
getNames (TypeDef {}) = []
getNames _  = []

-- Assigned names don't count, as they are monomorphic only
getStmtNames :: Statement NodeData -> [(String, NodeData)]
getStmtNames (Assign { assignExpr = expr }) = getExprNames expr
getStmtNames (If { ifCond = cond,
                   ifThen = thenStmt,
                   ifElse = elseStmt }) = getExprNames cond
                                       ++ getStmtNames thenStmt
                                       ++ concat (maybeToList 
                                                 (fmap getStmtNames elseStmt))
getStmtNames (While { whileCond = cond
                    , whileBody = body }) = getExprNames cond
                                         ++ getStmtNames body
getStmtNames (CallStmt { stmtExpr = expr }) = getExprNames expr
getStmtNames (Return { returnExpr = expr }) = getExprNames expr
getStmtNames (Block { blockBody = body}) = concat $[getStmtNames s | Stmt s <- body]
-- #TODO when we support closures this could be tricky
--   how do we specialize polymorphic closures?


getExprNames :: Expr NodeData -> [(String, NodeData)]
getExprNames (Call { callFunc = f
                   , callParams = params }) = getExprNames f 
                                           ++ concatMap getExprNames params
getExprNames (Identifier { exprNodeData = nodeData
                         , idName = name }) = [(name, nodeData)]
getExprNames (Literal {}) = []


prettyDef :: Definition NodeData -> Doc
prettyDef = undefined


checkTypes :: Module NodeData -> Inferrer ()
checkTypes (Module _ defs) = return () --mapM_ checkSignatures defs

-- #TODO figure out how we want to handle signatures, the errors we want, etc..
-- actually.. move them into the enviroment
--lookupSigType :: [Definition NodeData] -> String -> Maybe AST.TypeSignature
--lookupSigType defs name = lookup name $ concatMap sigNames defs 
--  where
--    sigNames (DefSignature _ _ names sig) = zip names $ repeat sig
--    sigNames _ = []


--getSigType :: [Definition NodeData] -> Definition NodeData -> Inferrer TypeRef
--getSigType defs (VariableDef { variable = Variable { varName = name } }) = getSig defs name
--getSigType defs (FunctionDef { functionName = name }) = getSig defs name
--getSigType defs (ForeignDef  { nativeDefName = name }) = getSig defs name
--getSigType _ _ = newType $ Var Nothing []

--getSig :: [Definition NodeData] -> String -> Inferrer TypeRef
--getSig defs name = case lookupSigType defs name of
--  Just sig -> convertTypeSig sig
--  Nothing -> newType $ Var Nothing []
{-
checkSignatures :: [Definition NodeData] -> Definition NodeData -> Inferrer ()
checkSignatures defs def@(VariableDef { defnNodeData = nodeData
                                      , variable = Variable { varName = name } }) 
        = checkSig def (lookupSigType defs name) (typeRef nodeData)
checkSignatures defs def@(FunctionDef { defnNodeData = nodeData
                                 , functionName = name })
        = checkSig def (lookupSigType defs name) (typeRef nodeData)
checkSignatures _ _ = return ()

addSigError :: String 
            -> Definition NodeData -> TypeRef -> TypeRef -> Inferrer ()
addSigError msg def expected inferred = do
          pExpected <- prettyTypeRef expected
          pInferred <- prettyTypeRef inferred
          let longmsg = 
                text "Expected type" <> colon <+> pExpected $+$
                text "Inferred type" <> colon <+> pInferred $+$
                text "In definition" <> colon <+> prettyDef def
          addError $ prettyError (defnPos def) msg longmsg

checkSig :: Definition NodeData -> Maybe AST.TypeSignature -> TypeRef -> Inferrer ()
checkSig def (Just typeSig) t = do
  verifyType <- convertTypeSig typeSig
  sigPassed <- typeEq t verifyType
  when (not sigPassed) $ 
    addSigError "Type does not satisfy signature" def verifyType t
checkSig _ _ _ = return ()

-}
{-


can I memoize the inferrer?

initialize everything to a  variable
top level statment  I need to store a reference of toplevel types

what are toplevel typerefs going to point to?
toplevel statments need werid things

need a function to lift unifies to the inferrer monad

-}

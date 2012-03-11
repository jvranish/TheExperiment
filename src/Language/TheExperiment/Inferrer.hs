{-#Language GeneralizedNewtypeDeriving
          , StandaloneDeriving
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable
          , FlexibleContexts
          , UndecidableInstances
          #-}
module Language.TheExperiment.Inferrer where

import Text.Parsec.Pos

import Control.Applicative hiding (empty)
import Control.Monad hiding (mapM, mapM_, forM)
import Control.Monad.Trans.State
import Control.Monad.Trans

import Data.List (genericLength)
import Data.Maybe
import Data.Foldable
import Data.Traversable

import Data.FixedList hiding (length)
import Data.Char
import qualified Data.List as List

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import qualified Data.Map as Map

import Language.TheExperiment.Misc
import Language.TheExperiment.Type
import Language.TheExperiment.AST hiding (TypeName)
import Language.TheExperiment.PrettyPrint

-- #TODO (this is a workaround for the naming clash between Type.hs and AST.hs)
--  fix this
import qualified Language.TheExperiment.AST as AST 

import Language.TheExperiment.CodeGenType

import qualified Control.Monad.GraphT as GraphT
import qualified Control.Monad.ErrorM as ErrorM

import Prelude hiding (concat, concatMap, mapM, mapM_)

import Debug.Trace

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

-- #TODO
-- I'll probably want to make the typeref an explicit element of 
-- expressions, hmmmm maybe....  perhaps if I wrapped it in a Maybe? nope
--  I need to update it on the fly
--  perhaps use a nodeData ref instead of tagging with the environment
--  hmmm, then I can easily update everything

data InferrerStatus = Inferred | Inferring

data Environment = Environment 
        { valueEnv :: Map.Map String (TopLevelStmt NodeData)
        -- , typeEnv  :: Map.Map String (TypeDef v)
        }

instance Show Environment where
  show (Environment a) = "Environment: " ++ show (fmap asdf $ Map.toList a)

asdf (k, v) = (show k, showStuff v)

type NodeId = Int

data TypeRef = TypeRef { unwrapTypeRef :: (GraphT.GraphRef Type) }

instance Show TypeRef where
  show _ = "TypeRef"

data NodeData = NodeData { nodeEnv :: Environment 
                         , typeRef :: TypeRef
                         , nodeId  :: NodeId
                         }
    deriving (Show)


showStuff (TopVarDef {}) = "VarDef"
showStuff (FuncDef {}) = "FuncDef"
showStuff (Foreign {}) = "Foreign"
showStuff (TypeDef {}) = "TypeDef"





data EitherF a = EitherF (Either String (Type a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

prettyEitherF a = ppParsedType $ convertToParsedType a

deriving instance Foldable (Either e) -- neatest feature ever.
deriving instance Traversable (Either e)

convertToParsedType (Y (EitherF (Left name))) = AST.TypeVariable undefined name
convertToParsedType (Y (EitherF (Right (TypeName name)))) =  AST.TypeName undefined name
convertToParsedType (Y (EitherF (Right (Var (Just name) NotOverloaded)))) =  AST.TypeVariable undefined name
convertToParsedType (Y (EitherF (Right (Std stdType)))) =  AST.TypeName undefined (show stdType)
convertToParsedType (Y (EitherF (Right (Func params ret)))) =  AST.FunctionType undefined (fmap convertToParsedType params) (convertToParsedType ret)


showType :: TypeRef -> Inferrer String
showType (TypeRef ref)  = Inferrer $ lift $ showType' ref

showType' :: (Monad m) => GraphT.GraphRef Type -> GraphT.GraphT Type m String
showType' ref = do
  (a, t) <- flattenType ref
  return $ (render $ prettyEitherF a)  ++ " : " ++ (List.intercalate ", " $ fmap showVarDef $ Map.assocs t)

showVarDef :: (String, Y EitherF) -> String
showVarDef (s, a) = s ++ " = " ++ (render $ prettyEitherF a)

data Y f = Y { unY :: (f (Y f)) }
{-}
instance (Pretty (f (Y f))) => Pretty (Y f) where
  pPrintPrec l p (Y a) = pPrintPrec l p a
-}
--flattenType :: (Monad m, Traversable f) => ContextRef t -> ContextT s (f (ContextRef s)) m (Y (EitherF f), Map.Map String (Y (EitherF f)))
flattenType :: (Monad m) => GraphT.GraphRef Type -> GraphT.GraphT Type m (Y EitherF, Map.Map String (Y EitherF))
flattenType ref = GraphT.forkMappedContext (ref :. Nil) (EitherF . Right) $ \(ref' :. Nil) ->
    evalStateT (runStateT (flatten ref') Map.empty) varNames


flatten :: (Monad m) => GraphT.GraphRef EitherF -> StateT (Map.Map [Char] (Y EitherF)) (StateT [String] (GraphT.GraphT EitherF m)) (Y EitherF)
flatten ref = do
    hasCycle <- lift $ lift $ GraphT.reachableFrom ref ref
    a <- lift $ lift $ GraphT.readRef ref
    case a of
      EitherF (Right (Var _ _)) -> liftM Y $ writeName ref =<< lift genSym
      _ -> case hasCycle of
          True -> do {- replace node with new symbol, add symbol to map, fill out that row in the map -}
            newSym <- liftM (fmap toUpper) $ lift genSym
            x <- writeName ref newSym
            flatA <- mapM flatten a
            modifyT $ Map.insert newSym $ Y flatA
            return $ Y x
          False -> liftM Y $ mapM flatten a
  where
    writeName ref' varName = do
      let x = EitherF $ Left varName
      lift $ lift $ GraphT.writeRef ref' x
      return x





prepareAST :: (Traversable f) => f a -> StateT Int Inferrer (f (TypeRef, NodeId))
prepareAST t = forM t $ \_ -> do
  ref <- lift $ newType $ Var Nothing NotOverloaded
  n <- getAndModifyT (+1)
  return (ref, n)


runInferrer :: Inferrer a -> ErrorM.ErrorM a
runInferrer (Inferrer m) = GraphT.runGraphT (evalStateT m (Map.empty))

infer :: Module a -> ErrorM.ErrorM (Module GenType)
infer m@(Module pos _) = runInferrer $ do
  m' <- evalStateT (prepareAST m) 0
  let m'' = scopeModule (Environment Map.empty) m'
  inferModule m'' -- (traceShow m'' m'')
  checkTypes (traceShow m'' m'')
  topStmts <- fmap (fmap snd) $ execStateT (specializeModule m'') []
  mapM convertNodeData (Module pos topStmts)



newtype Inferrer a = 
    Inferrer (StateT (Map.Map NodeId InferrerStatus) (GraphT.GraphT Type ErrorM.ErrorM) a)
  deriving (Monad, Applicative, Functor)

newType :: Type TypeRef -> Inferrer TypeRef
newType a = Inferrer $ liftM TypeRef $ lift $
               GraphT.newRef $ fmap unwrapTypeRef a
readType :: TypeRef -> Inferrer (Type TypeRef)
readType (TypeRef ref) = Inferrer $ liftM (fmap TypeRef) $ lift $
  GraphT.readRef ref

writeType :: TypeRef -> Type TypeRef -> Inferrer ()
writeType (TypeRef ref) a = Inferrer $ lift $
  GraphT.writeRef ref $ fmap unwrapTypeRef a

subsType :: TypeRef -> TypeRef -> Inferrer ()
subsType (TypeRef this) (TypeRef withThis) = Inferrer $ lift $
  GraphT.subsRef this withThis

copyType :: TypeRef -> Inferrer TypeRef
copyType (TypeRef ref) = Inferrer $ lift $ liftM TypeRef $ GraphT.copySubGraph ref

copyWithNewTypes :: (Traversable f) => f NodeData -> Inferrer (f NodeData)
copyWithNewTypes a = Inferrer $ lift $ GraphT.copySubGraphs lense a
  where
    lense n = (unwrapTypeRef $ typeRef n, \t -> n {typeRef = TypeRef t})

typeEq :: TypeRef -> TypeRef -> Inferrer Bool
typeEq (TypeRef a) (TypeRef b) = Inferrer $ lift $ GraphT.graphEq a b

refEq :: TypeRef -> TypeRef -> Inferrer Bool
refEq (TypeRef a) (TypeRef b) = Inferrer $ lift $ GraphT.refEq a b


addError :: Doc -> Inferrer ()
addError d = Inferrer $ lift $ lift $ ErrorM.addError d

getInferStatus :: TopLevelStmt NodeData -> Inferrer (Maybe InferrerStatus)
getInferStatus topStmt = 
  Inferrer $ liftM (Map.lookup (nodeId $ topStmtNodeData topStmt)) get 

setInferStatus :: TopLevelStmt NodeData -> InferrerStatus -> Inferrer ()
setInferStatus topStmt status = 
  Inferrer $ modify $ Map.insert (nodeId $ topStmtNodeData topStmt) status




localEnv :: Environment -> [(String, TopLevelStmt NodeData)] -> Environment
localEnv (Environment a) xs = Environment $ Map.union (Map.fromList xs) a

localDefs :: Environment -> [TopLevelStmt NodeData] -> Environment
localDefs env defs = localEnv env $ 
    fmap (\stmt -> (getTopLevelName stmt, stmt)) defs

getTopLevelName :: TopLevelStmt NodeData -> String
getTopLevelName (TopVarDef { varDef      = def  }) = varName def
getTopLevelName (FuncDef   { funcName    = name }) = name
getTopLevelName (Foreign   { foreignName = name }) = name
getTopLevelName (TypeDef   { typeDefName = name }) = name

applyScope :: (Functor f) => Environment -> f (TypeRef, NodeId) -> f NodeData
applyScope env a = fmap (\(ref, nId) -> NodeData env ref nId) a

scopeStatement :: Environment -> Statement (TypeRef, NodeId)
               -> Statement NodeData
scopeStatement env a@(Assign {})   = applyScope env a
scopeStatement env a@(If {})       = applyScope env a
scopeStatement env a@(While {})    = applyScope env a
scopeStatement env a@(ExprStmt {}) = applyScope env a
scopeStatement env a@(Return {})   = applyScope env a
scopeStatement env (Block pos (t, nId) (tlStmts, stmts)) = let
    env'     = localDefs env tlStmts'
    tlStmts' = fmap (scopeTopLevelStmt env') tlStmts
    stmts'   = fmap (scopeStatement env') stmts
    in Block pos (NodeData env' t nId) (tlStmts', stmts')

-- #TODO this needs to get ironed out with the return for functions
scopeTopLevelStmt :: Environment -> TopLevelStmt (TypeRef, NodeId)
                  -> TopLevelStmt NodeData
scopeTopLevelStmt env (FuncDef pos (ref, nId) name params ret stmt sig)
  = let        
    params' = fmap (applyScope env) params
    -- we could potentially handle the return type manually here and not
    -- have it explicitly in the AST but then we'd have to fake nodeId and such
    -- which might mess things up (and be ugly)
    ret' = applyScope env ret

    stmt'  = scopeStatement env' stmt
    env'    = localDefs env $ fmap topVar params'
    topVar a@(VarDef pos' nodeData _) = TopVarDef pos' nodeData a Nothing
    in FuncDef pos (NodeData env' ref nId) name params' ret' stmt' sig
scopeTopLevelStmt env a@(TypeDef {})   = applyScope env a 
scopeTopLevelStmt env a@(TopVarDef {}) = applyScope env a 


scopeModule :: Environment -> Module (TypeRef, NodeId)
            -> Module NodeData
scopeModule env (Module pos stmts) = let 
    -- we can do this because laziness is awesome
    env' = localDefs env stmts'
    stmts' = fmap (scopeTopLevelStmt env') stmts
    in Module pos stmts'



convertTypeSig :: TypeSig -> Inferrer TypeRef
convertTypeSig (TypeSig constraints t) = flip evalStateT Map.empty $ do
  table <- mapM makeConstraint constraints
  put $ Map.fromList table
  convertParsedType t

makeConstraint :: TypeConstraint
               -> StateT (Map.Map String TypeRef) Inferrer (String, TypeRef)
makeConstraint (TypeConstraint name constraints) = do
  constraintRefs <- mapM convertParsedType constraints
  var <- lift $ newType $ Var (Just name) (Overloads constraintRefs)
  return (name, var)

convertParsedType :: ParsedType
                  -> StateT (Map.Map String TypeRef) Inferrer TypeRef
convertParsedType (AST.TypeName { typeName = name }) 
-- #TODO add more stuff here (maybe use Read?)
  | name == "UInt32" = lift $ newType $ Std $ IntType UInt32
  | otherwise = lift $ newType $ TypeName name
convertParsedType (TypeVariable { typeVariable = name }) = do
  StateT $ \table -> do
    case Map.lookup name table of
      Just ref -> return (ref, table)
      Nothing  -> do
        ref <- newType $ Var (Just name) NotOverloaded
        return (ref, Map.insert name ref table)
-- #TODO
-- convertParsedType (TypeCall {  })  unsupported for the moment
convertParsedType (FunctionType { argTypes = args 
                                , returnType  = ret }) = do
  argRefs <- mapM convertParsedType args
  retRef <- convertParsedType ret
  lift $ newType $ Func argRefs retRef

copySigType :: TopLevelStmt NodeData -> Inferrer TypeRef
copySigType (TopVarDef { sig = Just typeSig }) = convertTypeSig typeSig
copySigType (FuncDef { sig = Just typeSig }) = convertTypeSig typeSig
copySigType (Foreign { sig = Just typeSig }) = convertTypeSig typeSig
copySigType _ = newType $ Var Nothing NotOverloaded

      
prettyError :: SourcePos -> String -> Doc -> Doc
prettyError pos msg longmsg = text "Error in" <+> text (show pos) <> colon <+> text msg $+$ nest 2 longmsg

prettyTypeRef :: TypeRef -> Inferrer Doc
prettyTypeRef _ = return $ text "Look at me! I'm a pretty type!"
prettyExpr :: Expr a -> Doc
prettyExpr _ =  text "Look at me! I'm a pretty expression!"
prettyDef :: TopLevelStmt a -> Doc
prettyDef _ =  text "Look at me! I'm a pretty definition!"

-- reference expr, expected, inferred
unify :: Expr NodeData -> TypeRef -> TypeRef -> Inferrer TypeRef
unify expr expected inferred = do
        sameRef <- refEq expected inferred
        case sameRef of
          True -> success
          False -> do 
            expected' <- readType expected
            inferred' <- readType inferred
            unifiedType <- unify' expected' inferred'
            subsType expected unifiedType
            subsType inferred unifiedType
            return unifiedType
    where
        unify' (TypeName a)  (TypeName b) | a == b = success
        unify' (Std a)       (Std b)      | a == b = success
        unify' (NType a)     (NType b)    | a == b = success
        unify' (Pointer a)   (Pointer b) = do
            aUb <- unify expr a b
            newType $ Pointer aUb
        unify' (Array a1 a2) (Array b1 b2) = do
            a1Ub1 <- unify expr a1 b1
            a2Ub2 <- unify expr a2 b2
            newType $ Array a1Ub1 a2Ub2
        unify' (Func aParams aRet) (Func bParams bRet) = do
            when (length aParams /= length bParams) $ do
              addUnifyError "Mismatch in number of parameters."
            params <- unifyList aParams bParams
            ret <- unify expr aRet bRet
            newType $ Func params ret
        unify' (Var aName aOverloads) 
               (Var bName bOverloads) = do
            overloads <- mergeOverloads aOverloads bOverloads
            case overloads of
              Overloads [x] -> return x
              _ -> newType $ Var (mplus aName bName) overloads
        unify' (Var _ _)       _         = return inferred
        unify' _               (Var _ _) = return expected
        unify' _               _         = failure

        mergeOverloads (Overloads as) (Overloads bs) = do
            us <- intersectByM typeEq as bs
            case us of
                [] -> do
                    addUnifyError "Could not find compatible overload."
                    return NotOverloaded
                _ -> return $ Overloads us
        mergeOverloads NotOverloaded b = return b
        mergeOverloads a NotOverloaded = return a

        unifyList (x:xs) (y:ys) = do
            u <- unify expr x y
            us <- unifyList xs ys
            return (u:us)
        unifyList _ _ = return []

        success = return expected
        failure = do
          addUnifyError "Could not unify types."
          return expected

        addUnifyError msg = do
          pExpected <- prettyTypeRef expected
          pInferred <- prettyTypeRef inferred
          let longmsg = 
                text "Expected type" <> colon <+> pExpected $+$
                text "Inferred type" <> colon <+> pInferred $+$
                text "In expression" <> colon <+> prettyExpr expr
          addError $ prettyError (exprPos expr) msg longmsg
--Error in "myfoo.hs" (line 123, column 21): Could not unify types.



lookupType :: SourcePos -> Environment -> String -> Inferrer TypeRef
lookupType pos (Environment env) name = do 
  case Map.lookup name env of
    Nothing -> do
      addError $ prettyError pos ("Not in scope: " ++ name) empty
      newType $ Var Nothing NotOverloaded
    Just topStmt -> inferTopLevelStmt topStmt 


inferExpr :: Expr NodeData -> Inferrer TypeRef
inferExpr expr = let
    t = typeRef $ exprNodeData expr
    in
    case expr of 
        Call { callFunc   = f
             , callParams = params } -> do

            f' <- inferExpr f
            params' <- mapM inferExpr params
            retType <- newType $ Var Nothing NotOverloaded
            inferredF <- newType $ Func params' retType
            unify expr f' inferredF
        Identifier { idName = name } -> do
            a <- lookupType (exprPos expr) (nodeEnv $ exprNodeData expr) name -- do I put the inferrer in the lookup?
            unify expr t a
        Literal { literal = lit } -> do
            case lit of
                StringLiteral s -> do
                    elementType <- newType $ Std Char8
                    sizeType <- newType $ NType $ genericLength s
                    newType $ Array sizeType elementType
                CharLiteral _ -> newType $ Std Char8
                FloatLiteral _ _ -> do
                    doubleType <- newType $ Std F64
                    floatType <- newType $ Std F32
                    newType $ Var Nothing $ Overloads [doubleType, floatType]
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
          newType $ Var Nothing NotOverloaded
        _ -> newType $ Var Nothing $ Overloads allowableTypes
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
            _ <- unify expr a b
            return ()
        If { ifCond = condExpr
           , ifThen = thenStmt
           , ifElse = elseStmtM } -> do
            condType <- inferExpr condExpr
            _ <- unify condExpr boolType condType
            inferStmt thenStmt
            mapM_ inferStmt elseStmtM
        While { whileCond = condExpr
              , whileBody = bodyStmt } -> do
            condType <- inferExpr condExpr
            _ <- unify condExpr boolType condType
            inferStmt bodyStmt
            return ()
        ExprStmt { stmtExpr = expr } -> do
            _ <- inferExpr expr
            return ()
        Return { returnExpr = expr } -> do
            a <- lookupName returnId
            b <- inferExpr expr
            _ <- unify expr a b
            return ()
        Block { blockBody = (topStmts, stmts)} -> do
            mapM_ inferTopLevelStmt topStmts
            mapM_ inferStmt stmts
    writeType t (Std Void) -- hmmmm

inferTopLevelStmt :: TopLevelStmt NodeData -> Inferrer TypeRef
inferTopLevelStmt topStmt = do
  let t = typeRef $ topStmtNodeData topStmt
  status <- getInferStatus topStmt
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
      setInferStatus topStmt Inferring
      -- inject signature type here before any actual type has been inferred
      -- if there is no signature, this will have no effect
      sigType <- copySigType topStmt
      sigStr <- showType sigType
      tStr <- showType t
      let errorString = sigStr ++ "\n" ++ tStr ++ "\n"
      unify (error ("signature unify error (impossible):\n" ++ errorString)) sigType t
      inferredType <- inferTopLevelStmtMemo topStmt
      setInferStatus topStmt Inferred
      pickPolyMono inferredType
  where
    pickPolyMono t = do
      case topStmt of
        -- For functions, copy the type. The caller will unify with the 
        --  copied type which will keep the function type polymorphic
        FuncDef { } -> copyType t
        Foreign { } -> copyType t
        -- Everything else we want monomorphic
        _ -> return t

inferTopLevelStmtMemo :: TopLevelStmt NodeData -> Inferrer TypeRef
inferTopLevelStmtMemo topStmt = do
    let t = typeRef $ topStmtNodeData topStmt
    case topStmt of
        TopVarDef { } -> do
            -- this doesn't do anything currently, but will when there are
            --  initialization expressions 
            return ()
        FuncDef { funcStmt   = stmt
                , funcParams = params } -> do
            inferStmt stmt
            let paramTypes = fmap (typeRef . varDefNodeData) params
            retType <- lookupType (stmtPos stmt) (nodeEnv $ stmtNodeData stmt) returnId 
            --funcType <- newType $ 
            --retType <- newType $ Var Nothing NotOverloaded
            writeType t $ Func paramTypes retType --funcType
        Foreign { } -> return ()
        -- hmmmm, what should I do here?
        TypeDef { } -> return ()
    return t

inferModule :: Module NodeData -> Inferrer ()
inferModule (Module _ topStmts) = mapM_ inferTopLevelStmt topStmts

getFuncName :: TopLevelStmt NodeData -> Maybe (String, NodeData)
getFuncName (FuncDef { funcName        = name
                     , topStmtNodeData = nodeData }) = Just (name, nodeData)
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
      Func _ _  -> convertError "we do not support function types yet"
      Var _ _   -> convertError "Something is broken. Var in output"
  where
    convertError msg = do
      addError $ text msg
      return $ GenType (CTypeName "error") []


specializeModule :: Module NodeData -> StateT [(NodeId, TopLevelStmt NodeData)] Inferrer ()
specializeModule (Module _ topStmts) = 
  mapM_ (uncurry specialize) (catMaybes $ fmap getFuncName topStmts) 

-- #TODO foreign functions cannot be specialized.... so we should 
--   add some protections around them or something



specialize :: String -> NodeData -> StateT [(NodeId, TopLevelStmt NodeData)] Inferrer ()
specialize name (NodeData { nodeEnv = Environment env
                          , typeRef = t }) = do
    a <- lookupDef
    case a of
      Just f@(FuncDef {}) -> do
        f' <- lift $ copyWithNewTypes(f)
        t' <- lift $ copyType t
        -- This will cause a horrible crash if there is a unify failure
        -- #TODO refactor how I do error checking in unify
        _ <- lift $ unify (error "error in specialize") t' (typeRef $ topStmtNodeData f')
        let names = getNames f'
        modify $ ((nodeId $ topStmtNodeData f', f') :)
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
          let potentials = lookups (nodeId $ topStmtNodeData f) specialized
          a <- lift $ anyM (typeEq t) 
                           (fmap (typeRef . topStmtNodeData) potentials)
          case a of
            True -> return Nothing
            False -> return $ Just f



getNames :: TopLevelStmt NodeData -> [(String, NodeData)]
getNames (TopVarDef {}) = []
getNames (FuncDef { funcStmt = stmt }) = getStmtNames stmt
getNames (TypeDef {}) = []

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
getStmtNames (ExprStmt { stmtExpr = expr }) = getExprNames expr
getStmtNames (Return { returnExpr = expr }) = getExprNames expr
getStmtNames (Block { blockBody = (_, stmts)}) =
-- #TODO when we support closures this could be tricky
--   how do we specialize polymorphic closures?
  concatMap getStmtNames stmts

getExprNames :: Expr NodeData -> [(String, NodeData)]
getExprNames (Call { callFunc = f
                   , callParams = params }) = getExprNames f 
                                           ++ concatMap getExprNames params
getExprNames (Identifier { exprNodeData = nodeData
                         , idName = name }) = [(name, nodeData)]
getExprNames (Literal {}) = []



checkTypes :: Module NodeData -> Inferrer ()
checkTypes (Module _ topStmts) = mapM_ checkSignatures topStmts

checkSignatures :: TopLevelStmt NodeData -> Inferrer ()
checkSignatures def@(TopVarDef { topStmtNodeData = nodeData
                           , sig             = Just typeSig }) 
        = checkSig def typeSig (typeRef nodeData)
checkSignatures def@(FuncDef { topStmtNodeData = nodeData
                         , sig             = Just typeSig })
        = checkSig def typeSig (typeRef nodeData)
checkSignatures _ = return ()

addSigError :: String 
            -> TopLevelStmt NodeData -> TypeRef -> TypeRef -> Inferrer ()
addSigError msg def expected inferred = do
          pExpected <- prettyTypeRef expected
          pInferred <- prettyTypeRef inferred
          let longmsg = 
                text "Expected type" <> colon <+> pExpected $+$
                text "Inferred type" <> colon <+> pInferred $+$
                text "In definition" <> colon <+> prettyDef def
          addError $ prettyError (topStmtPos def) msg longmsg

checkSig :: TopLevelStmt NodeData -> TypeSig -> TypeRef -> Inferrer ()
checkSig def typeSig t = do
  verifyType <- convertTypeSig typeSig
  sigPassed <- typeEq t verifyType
  when (not sigPassed) $ 
    addSigError "Type does not satisfy signature" def verifyType t

{-


can I memoize the inferrer?

initialize everything to a  variable
top level statment  I need to store a reference of toplevel types

what are toplevel typerefs going to point to?
toplevel statments need werid things

need a function to lift unifies to the inferrer monad

-}
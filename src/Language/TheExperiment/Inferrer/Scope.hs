module Language.TheExperiment.Inferrer.Scope where

import Data.Either
import Data.Monoid
import Control.Monad

import qualified Data.Map as Map

import Language.TheExperiment.Inferrer.InferrerM
import Language.TheExperiment.AST.Statement
import Language.TheExperiment.AST.Module
import Language.TheExperiment.AST.Type
import Language.TheExperiment.Misc

type Def = Definition NodeData
type Stmt = Statement NodeData
type DefOrStmt = DefOrStatement NodeData

data Env = Env 
        { valueEnv          :: Map.Map String Def
        , enclosingFunction :: Maybe Def
        } 

data NodeData = NodeData { nodeEnv :: Env 
                         , typeRef :: TypeRef
                         , nodeId  :: NodeId
                         , typeSig :: Maybe TypeSignature
                         }

instance Monoid Env where
  mempty = Env Map.empty Nothing
  mappend (Env aEnv aEnc) (Env bEnv bEnc) = 
      Env (Map.union bEnv aEnv) (aEnc `mplus` bEnc)  

mkEnv :: Maybe Def -> [(String, Def)] -> Env
mkEnv md xs = Env (Map.fromList xs) md

localEnv :: Env -> Maybe Def -> [Def] -> Env
localEnv env f defs = env `mappend` (mkEnv f $ selectAndPair getDefName defs)


getDefName :: Definition a -> Maybe String
getDefName (VariableDef { variable      = def  }) = Just $ varName def
getDefName (FunctionDef { functionName  = name }) = Just name
getDefName (ForeignDef  { nativeDefName = name }) = Just name
getDefName _                                     = Nothing


applyScope :: (Functor f) => Env -> f (TypeRef, NodeId) -> f NodeData
applyScope env a = fmap (\(ref, nId) -> NodeData env ref nId Nothing) a

scopeStatement :: Env -> Statement (TypeRef, NodeId) -> Stmt
scopeStatement env a = case a of
    Assign {}                  -> a'
    If { ifThen = t
       , ifElse = e }          -> a' { ifThen = scopeStatement env t
                                     , ifElse = fmap (scopeStatement env) e }
    While { whileBody = body } -> a' { whileBody = scopeStatement env body }
    CallStmt {}                -> a'
    Return {}                  -> a'
    Block { blockBody = body } -> a' 
      { blockBody = let
        -- super awesome laziness in action
        -- This is pairing each statement with the list of statements that 
        -- come before it. This is so that 'var x' scopes 'x' only for
        -- statements that come after it.
        body' = zipWith scopeDefOrStmt body $ inits body'
        in body' }
  where
    a' = applyScope env a
    scopeDefOrStmt x beforeX = case x of
        Def  def  -> Def $ scopeDefinition env' sigTable def
        Stmt stmt -> Stmt $ scopeStatement env' stmt
      where
        env' = localEnv env Nothing $ lefts $ fmap defOrStmtToEither beforeX
        sigTable = concat $ 
           [zip names $ repeat sig | Def (DefSignature _ _ names sig)  <- beforeX]

updateSig :: Def -> Maybe TypeSignature -> Def
updateSig a sig = a { defnNodeData = (defnNodeData a) { typeSig = sig }}
-- #TODO if there is a duplicate, error put an error somewhere!

scopeDefinition :: Env -> [(String, TypeSignature)] -> Definition (TypeRef, NodeId) -> Def
scopeDefinition env sigs a = case a of
    TypeDef {}      -> a'
    DefSignature {} -> a'
    VariableDef { variable = Variable { varName = name }}  -> 
        a' `withSigFor` name
    ForeignDef { nativeDefName = name }   -> a' `withSigFor` name
    FunctionDef { functionName = name
                , functionBlock = block } -> let
        env' = localEnv env 
                  (Just result)
                  (fmap promoteVar $ functionParams result)
        result = a' { functionBlock = scopeStatement env' block }
      in result `withSigFor` name

  where
    withSigFor x name = x `updateSig` lookup name sigs
      --where
      --  temp (Just z) = Just z
      --  temp (Nothing) = Just $ error name
    a' = applyScope env a

promoteVar :: Variable a -> Definition a
promoteVar var@(Variable pos nodeData _) = VariableDef pos nodeData var

scopeModule :: Env -> Module (TypeRef, NodeId) -> Module NodeData
scopeModule env (Module pos defs) = let 
    -- we can do this because laziness is awesome
    defs' = fmap (scopeDefinition (localEnv env Nothing defs') sigTable) defs
    in Module pos defs'
  where
    sigTable = concat $  [zip names $ repeat sig | DefSignature _ _ names sig <- defs]


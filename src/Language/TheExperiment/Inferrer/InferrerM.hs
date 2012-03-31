{-#Language GeneralizedNewtypeDeriving
  #-}
module Language.TheExperiment.Inferrer.InferrerM where

import qualified Data.Map as Map

import Control.Applicative

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Traversable

import Text.PrettyPrint.HughesPJ

import qualified Control.Monad.GraphT as GraphT
import qualified Control.Monad.ErrorM as ErrorM

import Language.TheExperiment.Inferrer.Type


data TypeRef = TypeRef { unwrapTypeRef :: (GraphT.GraphRef Type) }

instance Show TypeRef where
  show _ = "TypeRef"

type NodeId = Int

data InferrerStatus = Inferred | Inferring

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

copyWithNewTypes :: Traversable f => (a -> (TypeRef, TypeRef -> a)) -> f a -> Inferrer (f a)
copyWithNewTypes lense a = Inferrer $ lift $ GraphT.copySubGraphs lense' a
  where
    lense' x = let (t, g) = lense x in (unwrapTypeRef t, g . TypeRef)

typeEq :: TypeRef -> TypeRef -> Inferrer Bool
typeEq (TypeRef a) (TypeRef b) = Inferrer $ lift $ GraphT.graphEq a b

refEq :: TypeRef -> TypeRef -> Inferrer Bool
refEq (TypeRef a) (TypeRef b) = Inferrer $ lift $ GraphT.refEq a b


addError :: Doc -> Inferrer ()
addError d = Inferrer $ lift $ lift $ ErrorM.addError d

getInferStatus :: NodeId -> Inferrer (Maybe InferrerStatus)
getInferStatus nodeId = Inferrer $ liftM (Map.lookup nodeId) get

setInferStatus :: NodeId -> InferrerStatus -> Inferrer ()
setInferStatus nodeId status = Inferrer $ modify $ Map.insert nodeId status



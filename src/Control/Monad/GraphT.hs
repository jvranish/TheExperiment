{-#Language GeneralizedNewtypeDeriving #-}
module Control.Monad.GraphT where

import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.FixedList

import Control.Applicative
import Control.Monad hiding (mapM, mapM_, forM, forM_)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Fix

import qualified Control.Monad.ContextT as ContextT

import Prelude hiding (elem, and, concat, mapM, mapM_, foldr)


newtype GraphT f m a = GraphT { unGraphT :: ContextT.ContextT (f (GraphRef f)) m a }
 deriving (Monad, MonadFix, MonadTrans, Applicative, Alternative, Functor, MonadPlus, MonadIO)

newtype GraphRef f = GraphRef { unGraphRef :: ContextT.ContextRef (f (GraphRef f)) }

runGraphT :: (Monad m) => GraphT f m a -> m a
runGraphT m = ContextT.runContextT $ unGraphT m

mapGraphT :: (m (a, ContextT.ContextData (f (GraphRef f))) -> n (b, ContextT.ContextData (f (GraphRef f)))) -> GraphT f m a -> GraphT f n b
mapGraphT f (GraphT m) = GraphT $ ContextT.mapContextT f m

newRef :: (Monad m) => f (GraphRef f) -> GraphT f m (GraphRef f)
newRef a = liftM GraphRef $ GraphT $ ContextT.newRef a

readRef :: (Monad m) => GraphRef f -> GraphT f m (f (GraphRef f))
readRef (GraphRef ref) = GraphT $ ContextT.readRef ref

writeRef :: (Monad m) => GraphRef f -> f (GraphRef f) -> GraphT f m ()
writeRef (GraphRef ref) a = GraphT $ ContextT.writeRef ref a

subsRef :: (Monad m) => GraphRef f -> GraphRef f -> GraphT f m ()
subsRef (GraphRef a) (GraphRef b) = GraphT $ ContextT.subsRef a b

subsRefs :: (Monad m) => [GraphRef f] -> GraphRef f -> GraphT f m ()
subsRefs xs (GraphRef ref) = GraphT $ ContextT.subsRefs (fmap unGraphRef xs) ref

refEq :: (Monad m) => GraphRef f -> GraphRef f -> GraphT f m Bool
refEq (GraphRef a) (GraphRef b) = GraphT $ ContextT.refEq a b

unsafeCastRef :: (Functor f) => (f (GraphRef g) -> g (GraphRef g)) -> GraphRef f -> GraphRef g
unsafeCastRef f (GraphRef (ContextT.UnsafeContextRef ref a)) = GraphRef $ ContextT.UnsafeContextRef ref (f (fmap (unsafeCastRef f) a))

forkContext :: (Functor f, Functor t, Foldable t, Monad m) => t (GraphRef f) -> (t (GraphRef f) -> GraphT f m b) -> GraphT f m b
forkContext refs f = forkMappedContext refs id f
  
oneOf :: (Monad m) => [GraphT f m (Maybe a)] ->  GraphT f m (Maybe a)
oneOf ms = GraphT $ ContextT.UnsafeContextT $ StateT $ \context -> do
    let onlyOne (Nothing, _) a  = a
        onlyOne a (Nothing, _) = a
        onlyOne _ _ = (Nothing, context)
    return . foldr onlyOne (Nothing, context) =<< mapM (flip runStateT context . ContextT.unsafeUnContextT . unGraphT) ms

forkMappedContext :: (Monad m, Functor f, Functor t, Foldable t) => t (GraphRef f) -> (f (GraphRef g) -> g (GraphRef g)) -> (t (GraphRef g) -> GraphT g m b) -> GraphT f m b
forkMappedContext refs f g = do
  GraphT $ mapM_ ContextT.unsafeLookupRef (fmap unGraphRef refs) -- force ref commit
  context <- GraphT $ ContextT.UnsafeContextT $  get
  GraphT $ ContextT.UnsafeContextT $ lift $ evalStateT (ContextT.unsafeUnContextT $ unGraphT $ g $ fmap (unsafeCastRef f) refs) (fmap (f . fmap (unsafeCastRef f))  context)


refElem :: (Monad m, Foldable g) => GraphRef f -> g (GraphRef f) -> GraphT f m Bool
refElem ref t = refElem' $ toList t
  where
    refElem' []     = return False
    refElem' (x:xs) = do
      yep <- refEq ref x
      case yep of
        True  -> return True
        False -> refElem' xs

lookupRef :: (Monad m) => GraphRef f -> [(GraphRef f, a)] -> GraphT f m (Maybe a)
lookupRef _ []            = return Nothing
lookupRef ref ((x,y):xys) = do
  yep <- refEq ref x
  case yep of
    True  -> return $ Just y
    False -> lookupRef ref xys
    

copySubGraph :: (MonadFix m, Traversable f, Functor f) =>
                GraphRef f -> GraphT f m (GraphRef f)
copySubGraph ref = do
  relevantNodes <- reachable ref
  lookupNew <- copySubGraphWithRespectTo relevantNodes
  lookupNew ref


-- #TODO decide on final implementation
copySubGraphWithRespectTo
  :: (Traversable f, MonadFix m) =>
     [GraphRef f] -> GraphT f m (GraphRef f -> GraphT f m (GraphRef f))
copySubGraphWithRespectTo relevantNodes = do
  newNodes <- forM relevantNodes $ \x -> do
      newValue <- readRef x
      newRef newValue
  let lookupNew a = liftM fromJust $ lookupRef a $ zip relevantNodes newNodes

  forM_ newNodes $ \x -> do
      v <- readRef x
      writeRef x =<< mapM lookupNew v
  return lookupNew

{-
  mfix $ \lookupNew -> do
    newNodes <- forM relevantNodes $ \x -> do
      newValue <- readRef x
      newRef =<< mapM lookupNew newValue
    let lookupNew' a = liftM fromJust $ lookupRef a $ zip relevantNodes newNodes
    return lookupNew'
-}

copySubGraphs
  :: (Traversable f, Traversable t, MonadFix m, Functor m) =>
     (a -> (GraphRef f, GraphRef f -> a)) -> t a -> GraphT f m (t a)
copySubGraphs lense a = do
  relevantNodes <- fmap concat $ mapM (reachable . fst . lense) a
  lookupNew <- copySubGraphWithRespectTo relevantNodes
  mapM ((\(ref, g) -> fmap g $ lookupNew ref) . lense) a

-- 
-- Check to see if a is reachable from b. Will return false if a == b unless there is a cycle
reachableFrom :: (Foldable f, Monad m) => GraphRef f -> GraphRef f -> GraphT f m Bool
reachableFrom a b = refElem a =<< liftM concat . mapM reachable =<< return . toList =<< readRef b
  
-- The returned list always includes the original reference
reachable :: (Foldable f, Monad m) => GraphRef f -> GraphT f m [GraphRef f]
reachable ref = reachable' [] ref
reachable' :: (Monad m, Foldable f) => [GraphRef f] -> GraphRef f -> GraphT f m [GraphRef f]
reachable' xs ref= do
  alreadyFound <- refElem ref xs
  case alreadyFound of
    True -> return []
    False -> do
      x <- readRef ref
      xs' <- liftM concat $ mapM (reachable' (ref:xs)) $ toList x
      return (ref:xs')

graphEq :: (Functor f, Eq (f ()), Foldable f, Monad m) => GraphRef f -> GraphRef f -> GraphT f m Bool
graphEq aRef' bRef' = forkContext (aRef' :.  bRef' :. Nil) $ \(a' :.  b' :. Nil) -> let 
    graphEq' aRef bRef = do
      eq <- refEq aRef bRef
      case eq of
        True -> return True
        False -> do 
          a <- readRef aRef
          b <- readRef bRef
          case headEq a b of 
            False -> return False
            True -> do
              subsRef aRef bRef
              liftM and $ zipWithM graphEq' (toList a) (toList b)
    unitize x = fmap (const ()) x
    headEq a b = unitize a == unitize b
  in graphEq' a' b'
  
  

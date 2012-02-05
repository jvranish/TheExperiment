{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.MemoT where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Fix

import qualified Data.Map as Map

-- | I could replace this with Maybe, but I think this makes the code
--   more readable and using Maybe doesn't really simplify the code any.
data Cached a = Cached a
              | Computing

newtype MemoT k v m a = 
  MemoT { unMemoT :: (StateT (Map.Map k (Cached v)) m a) }
  deriving (Monad, MonadFix, MonadTrans, Applicative, Alternative, Functor, MonadPlus, MonadIO)

memo :: (MonadFix m, Ord k) => 
        (a -> k) -> (a -> m v) -> (a -> MemoT k v m v) -> a -> MemoT k v m v
memo toKey defaultAction f a = MemoT $ do
  let key = toKey a
  t <- get
  case Map.lookup (toKey a) t of
    Just (Cached v) -> return v
    Just Computing -> lift $ defaultAction a
    Nothing -> do
      put $ Map.insert key Computing t
      v <- unMemoT $ f a
      put $ Map.insert key (Cached v) t
      return v

runMemoT :: (Monad m) => MemoT k v m a -> m a
runMemoT (MemoT m) = evalStateT m Map.empty

{-}
newtype MemoIOT k v m a = MemoIOT { unMemoIOT :: (ReaderT (IORef (Map.Map k (Cached v))) m a) }
  deriving (Monad, MonadFix, MonadTrans, Applicative, Alternative, Functor, MonadPlus, MonadIO)

memoIO
  :: (MonadIO m, Ord k) =>
     (a -> m v) -> (a -> k) -> m v -> a -> MemoIOT k v m v
memoIO f toKey defaultValue a = MemoIOT $ do
  let key = toKey a
  ref <- ask -- this causes problems....
  t <- liftIO $ readIORef ref
  case Map.lookup (toKey a) t of
    Just (Cached v) -> return v
    Just Computing -> do
      v <- lift $ defaultValue
      return v
    Nothing -> do
      liftIO $ modifyIORef ref $ Map.insert key Computing
      v <- lift $ f a
      liftIO $ modifyIORef ref $ Map.insert key (Cached v)
      return v

-- test :: (Monad m) => MemoT Float Float m Float
--test x =   (memo (liftM foo . test) id (return 0.5)) x

foo :: Float -> Float
foo x = trace (show x) $ x * x - 5

test :: (MonadIO m, MonadFix m) => MemoIOT Float Float m Float
test = mfix $ memoIO (return . foo) id (return 0.5)

--runMEmoIOT :: 
runMemoIOT (MemoIOT m) = do
  ref <- liftIO $ newIORef Map.empty
  runReaderT m ref
  -}

{-
asdf f = do
  x <- f a
  asdf
-}




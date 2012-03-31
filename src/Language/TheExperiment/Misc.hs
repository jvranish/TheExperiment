module Language.TheExperiment.Misc where

import Control.Monad hiding (mapM)
import Control.Monad.Trans.State

import Data.Foldable
import Data.Traversable

import Prelude hiding (error, mapM, sequence, and, or)

-- This almost works, but does not short circuit
-- anyM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
-- anyM p = liftM or . mapM p

anyM :: (Traversable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM p t = anyM' p $ toList t
  where
    anyM' _ [] = return False
    anyM' p' (x:xs) = do
      eq <- p' x
      case eq of
        True -> return True
        False -> anyM' p' xs

intersectByM :: (Monad m, MonadPlus p) => (a -> a -> m Bool) -> [a] -> [a] -> m (p a)
intersectByM _ [] _ = return mzero
intersectByM _ _ [] = return mzero
intersectByM eq (x:xs) ys = do
  bool <- anyM (eq x) ys
  if bool
    then liftM (mplus $ return x) $ intersectByM eq xs ys
    else intersectByM eq xs ys

lookups :: (Eq a) => a -> [(a, b)] -> [b]
lookups k xs = fmap snd $ filter ((== k) . fst) xs

getT :: (Monad m) => StateT s m s
getT = StateT $ \s -> return (s, s)

putT :: (Monad m) => s -> StateT s m ()
putT s = StateT $ \_ -> return ((), s)

modifyT :: (Monad m) => (s -> s) -> StateT s m ()
modifyT f = do
  s <- getT
  putT (f s)

getAndModifyT :: (Monad m) => (s -> s) -> StateT s m s
getAndModifyT f = do
  a <- getT
  modifyT f
  return a


makeSupply :: [[a]] -> [[a]] -> [[a]]
makeSupply inits' tails = let vars = inits' ++ (liftM2 (++) vars tails) in vars

varNames :: [String]
varNames = makeSupply (words "a b c d e f g h i j k") (words "1 2 3 4 5")

genSym :: (Monad m) => StateT [a] m a
genSym = do
  x:xs <- getT
  putT xs
  return x

pair :: (a -> b) -> a -> (b, a)
pair f a = (f a, a)

selectAndPair :: (a -> Maybe b) -> [a] -> [(b, a)]
selectAndPair f xs = [(b, a) | (Just b, a) <- fmap (pair f) xs]

-- The inits function in the version of haskell that I'm using currently
-- does not appear to have the correct strictness properties. So I'm using
-- my own
inits                   :: [a] -> [[a]]
inits xs                =  [] : case xs of
                                  []      -> []
                                  x : xs' -> map (x :) (inits xs')

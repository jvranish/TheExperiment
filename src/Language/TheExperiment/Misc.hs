module Language.TheExperiment.Misc where

import Control.Monad hiding (mapM)

import Data.Foldable
import Data.Traversable

import Prelude hiding (error, mapM, sequence, and, or)

anyM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
anyM p = liftM or . mapM p

intersectByM :: (Monad m, MonadPlus p) => (a -> a -> m Bool) -> [a] -> [a] -> m (p a)
intersectByM _ [] _ = return mzero
intersectByM _ _ [] = return mzero
intersectByM eq (x:xs) ys = do
  bool <- anyM (eq x) ys
  if bool
    then liftM (mplus $ return x) $ intersectByM eq xs ys
    else intersectByM eq xs ys
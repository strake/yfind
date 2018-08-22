module Atomic (atoms, isAtomic) where

import Control.Monad (guard, when)
import Control.Monad.ST
import Data.Array
import Data.Filtrable
import Data.Foldable
import Data.Functor.Compose
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Traversable
import Data.UnionFind.ST
import Util.Array

isAtomic :: (Ix i, Foldable f) => (i -> f i) -> NonEmpty (Array i Bool) -> Bool
isAtomic nbhd = (< 2) . length . atoms nbhd

-- Compute all the non-interacting subpatterns of the given pattern.
-- All generations of pattern must be given.
atoms :: (Ix i, Foldable f) => (i -> f i) -> NonEmpty (Array i Bool) -> [NonEmpty (Array i Bool)]
atoms nbhd grids = runST ((fmap . fmap) getCompose . parts . Compose =<< atomize nbhd grids)

parts :: Traversable f => f (Bool, Point s ()) -> ST s [f Bool]
parts xs = do
    ps <- List.nub <$> (\ (a, p) -> (<$ guard a) <$> repr p) `mapMaybeA` toList xs
    ps `for` \ p -> xs `for` \ (a, q) -> (&&) a <$> equivalent p q

atomize :: (Ix i, Foldable f) => (i -> f i) -> NonEmpty (Array i Bool) -> ST s (NonEmpty (Array i (Bool, Point s ())))
atomize nbhd grids = do
    u:|us <- traverse (atomize1 nbhd) grids
    u:|us <$ foldlM (\ u v -> v <$ unifyNbhds nbhd True u v) u us

atomize1 :: (Ix i, Foldable f) => (i -> f i) -> Array i Bool -> ST s (Array i (Bool, Point s ()))
atomize1 nbhd grid = do
    u <- for grid $ (<$> fresh ()) . (,)
    u <$ unifyNbhds nbhd False u u

unifyNbhds :: (Ix i, Foldable f) => (i -> f i) -> Bool -> Array i (Bool, Point s ()) -> Array i (Bool, Point s ()) -> ST s ()
unifyNbhds nbhd b u v =
    (range . bounds) u `for_` \ i -> when (fst $ u ! i) $
    (nbhd i)           `for_` \ j -> when (not b || (fst $ v ! i)) $
    sequenceA_ [union p q | (_, p) <- u !? i
                          , (_, q) <- v !? j]

(<$>>=) :: Monad m => m a -> (a -> m ()) -> m a
am <$>>= f = (<*>) (<$) f =<< am

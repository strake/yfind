module Atomic (atoms, isAtomic) where

import Control.Monad (guard)
import Control.Monad.ST
import Data.Array
import Data.Bool
import Data.Filtrable
import Data.Foldable
import Data.Functor.Compose
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Traversable
import Data.UnionFind.ST
import Util
import Util.Array

isAtomic :: (Ix i, Foldable f) => (i -> f i) -> NonEmpty (Array i Bool) -> Bool
isAtomic nbhd = (< 2) . length . atoms nbhd

-- Compute all the non-interacting subpatterns of the given pattern.
-- All generations of pattern must be given.
atoms :: (Ix i, Foldable f) => (i -> f i) -> NonEmpty (Array i Bool) -> [NonEmpty (Array i Bool)]
atoms nbhd grids = runST ((fmap . fmap) getCompose . parts . Compose =<< atomize nbhd grids)

parts :: Traversable f => f (Bool, Point s ()) -> ST s [f Bool]
parts xs = do
    ps <- List.nub <$> (traverse repr . mapMaybe (\ (a, p) -> p <$ guard a) . toList) xs
    ps `for` \ p -> xs `for` \ (a, q) -> bool (pure False) (equivalent p q) a

atomize :: (Ix i, Foldable f) => (i -> f i) -> NonEmpty (Array i Bool) -> ST s (NonEmpty (Array i (Bool, Point s ())))
atomize nbhd grids = do
    u:|us <- traverse (atomize1 nbhd) grids
    u:|us <$ foldlM (\ u v -> v <$ unify nbhd False u v) u us

atomize1 :: (Ix i, Foldable f) => (i -> f i) -> Array i Bool -> ST s (Array i (Bool, Point s ()))
atomize1 nbhd grid = do
    u <- for grid $ (<$> fresh ()) . (,)
    u <$ unify nbhd True u u

unify :: (Ix i, Foldable f) => (i -> f i) -> Bool -> Array i (Bool, Point s ()) -> Array i (Bool, Point s ()) -> ST s ()
unify nbhd b u v = foldMapA id
    [union p q | (i, (True, p)) <- assocs u, j <- i : toList (nbhd i)
               , (a, q) <- toList (v !? j), a || b]

{-# LANGUAGE RecordWildCards #-}

module Evolve (evolve) where

import Prelude hiding (replicate)
import Control.Applicative
import Control.Monad
import Data.Array
import Data.Bool
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Product
import Data.Maybe (fromMaybe)
import Data.Traversable
import Util
import Util.Array
import Z3.Tagged

import Rule

evolve :: (Num i, Ix i, Applicative f, Traversable f) => Rule (i, i) f Bool -> Array (i, i) (AST s) -> Z3 s (Array (i, i) (AST s))
evolve (Rule {..}) = evolve' nbhd $ \ theActualNbhd ->
    [cell
       | cell <- mkFreshConst "cell" =<< mkBoolSort
       , () <- assert <=< mkOr <=< for (sequenceA $ pure [False, True]) $ \ theNbhd ->
               mkAnd . toList =<< traverse2 (bool mkNot pure) (Pair <*> Identity . evolveCell $ theNbhd)
                                                              (Pair theActualNbhd $ Identity cell)]

evolve' :: (Num i, Ix i, Applicative f)
        => ((i, i) -> f (i, i)) -> (f (AST s) -> Z3 s (AST s)) -> Array (i, i) (AST s) -> Z3 s (Array (i, i) (AST s))
evolve' nbhd evolveCell = \ a -> do
    false <- mkBool False
    let evolveNbhd = evolveCell . fmap (fromMaybe false . (a !?)) . nbhd
    -- boundary conditions
    for_ (range $ expandedBounds a) $ \ ix -> case a !? ix of
        Nothing -> assert =<< mkEq false =<< evolveNbhd ix
        Just _ -> pure ()
    liftA2 fmap array (traverse (liftA2 fmap (,) evolveNbhd) . range) (bounds a)
  where expandedBounds a = let ((il, jl), (ih, jh)) = bounds a
                           in ((il-1, jl-1), (ih+1, jh+1))

{-# LANGUAGE RecordWildCards #-}

module Evolve (evolve) where

import Prelude hiding (replicate)
import Control.Monad
import Data.Array
import Data.Bool
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Traversable
import Util
import Util.Array
import Z3.Tagged

import Rule

evolve :: (Num i, Ix i, Applicative f, Traversable f) => Rule (i, i) f Bool -> Array (i, i) (AST s) -> Z3 s (Array (i, i) (AST s))
evolve (Rule {..}) = \ a -> do
    false <- mkBool False
    true  <- mkBool True
    let evolveNbhd ix = mkOr <=< for (sequenceA $ pure [False, True]) $ \ theNbhd ->
                        bool (pure false) `flip` evolveCell theNbhd $
                        mkAnd . toList =<<
                        traverse2 mkEq (bool false true <$> theNbhd)
                                       (fromMaybe false . (a !?) <$> nbhd ix)
    x <- traverse (getSort >=> mkFreshConst "cell") a
    x <$ (range . expandedBounds) a `for` \ ix -> assert =<< mkEq (fromMaybe false $ x !? ix) =<< evolveNbhd ix
  where expandedBounds a = let ((il, jl), (ih, jh)) = bounds a
                           in ((il-1, jl-1), (ih+1, jh+1))

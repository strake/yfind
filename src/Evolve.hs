module Evolve (evolve) where

import Prelude hiding (replicate)
import Control.Monad
import Data.Array
import Data.Bits
import Data.Bits.Bitwise
import Data.Bool
import Data.Maybe (fromMaybe)
import Data.Rule.Hex
import Data.Traversable
import Util.Array
import Z3.Monad

import Nbhd

evolve :: (MonadZ3 z3, Num i, Ix i) => Rule -> Array (i, i) AST -> z3 (Array (i, i) AST)
evolve rule = \ a -> do
    false <- mkBool False
    true  <- mkBool True
    let evolveNbhd ix = mkOr <=< for [0..127] $ \ n ->
                        bool (pure false) `flip` testBit table n $
                        mkAnd =<<
                        zipWithM mkEq (bool false true <$> toListLE n)
                                      (fromMaybe false . (a !?) <$> rawHexNbhd ix)
    x <- traverse (getSort >=> mkFreshConst "cell") a
    x <$ (range . expandedBounds) a `for` \ ix -> assert =<< mkEq (fromMaybe false $ x !? ix) =<< evolveNbhd ix
  where table = tabulate rule
        expandedBounds a = let ((il, jl), (ih, jh)) = bounds a
                           in ((il-1, jl-1), (ih+1, jh+1))

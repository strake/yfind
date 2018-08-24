{-# LANGUAGE RecordWildCards #-}

module Evolve (evolve') where

import Prelude hiding (replicate)
import Control.Applicative
import Data.Array
import Data.Foldable
import Data.Maybe (fromMaybe)
import Util.Array
import Z3.Tagged

evolve' :: (Num i, Ix i, Applicative f)
        => ((i, i) -> f (i, i)) -> (f (AST s) -> Z3 s (AST s)) -> Array (i, i) (AST s) -> Z3 s (Array (i, i) (AST s))
evolve' nbhd evol = \ a -> do
    false <- mkBool False
    let evolveNbhd = evol . fmap (fromMaybe false . (a !?)) . nbhd
    -- boundary conditions
    for_ (range $ expandedBounds a) $ \ ix -> case a !? ix of
        Nothing -> assert =<< mkEq false =<< evolveNbhd ix
        Just _ -> pure ()
    liftA2 fmap array (traverse (liftA2 fmap (,) evolveNbhd) . range) (bounds a)
  where expandedBounds a = let ((il, jl), (ih, jh)) = bounds a
                           in ((il-1, jl-1), (ih+1, jh+1))

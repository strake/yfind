{-# LANGUAGE RecordWildCards #-}

module Evolve (mkEvol, evolve') where

import Prelude hiding (replicate)
import Control.Applicative
import Control.Monad
import Data.Array
import Data.Foldable
import Data.Maybe (fromMaybe)
import Util
import Util.Array
import Z3.Tagged

mkEvol :: (Applicative f, Traversable f) => Int -> (f Bool -> Bool) -> Z3 s (FuncDecl s)
mkEvol l evolveCell = do
    evol <- mkFreshFuncDecl "evolve" <$> take l . repeat <*> id =<< mkBoolSort
    evol <$ for_ (sequenceA $ pure [False, True]) (assert <=< bind2 mkEq <$> mkBool . evolveCell <*> (mkApp evol . toList <=< traverse mkBool))

evolve' :: (Num i, Ix i, Applicative f, Foldable f)
        => ((i, i) -> f (i, i)) -> FuncDecl s -> Array (i, i) (AST s) -> Z3 s (Array (i, i) (AST s))
evolve' nbhd evol = \ a -> do
    false <- mkBool False
    let evolveNbhd = mkApp evol . toList . fmap (fromMaybe false . (a !?)) . nbhd
    -- boundary conditions
    for_ (range $ expandedBounds a) $ \ ix -> case a !? ix of
        Nothing -> assert =<< mkEq false =<< evolveNbhd ix
        Just _ -> pure ()
    liftA2 fmap array (traverse (liftA2 fmap (,) evolveNbhd) . range) (bounds a)
  where expandedBounds a = let ((il, jl), (ih, jh)) = bounds a
                           in ((il-1, jl-1), (ih+1, jh+1))

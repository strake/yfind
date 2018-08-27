{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module YCommon where

import Prelude hiding (filter, head, last, replicate, id, (.))
import Control.Category
import Control.Monad
import Data.Array
import Data.Bool
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Product
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Traversable
import Data.Universe.Class
import Data.Universe.Instances.Base ()
import Numeric.Natural
import Util
import Util.Array
import Z3.Tagged

import Evolve
import Nbhd

mkExcludeGrids :: (Ix i, Foldable f) => f (Array i (AST s)) -> Array i Bool -> Z3 s (AST s)
mkExcludeGrids grids answer =
    mkAnd <=< for (toList grids) $
    mkNot <=< mkAnd . elems <=<
    zipArraysA (\ (fromMaybe False -> value) ->
                maybe (mkBool False) pure >=> \ ast ->
                mkEq ast =<< mkBool value) answer

mkExcludeSame :: Foldable f => f (Bool, AST s) -> Z3 s (AST s)
mkExcludeSame = mkNot <=< mkAnd <=< traverse (uncurry $ bool mkNot pure) . toList

setupRule :: ∀ nbhd s .
    (Applicative (Shape nbhd), Traversable (Shape nbhd), Neighborly nbhd, Cell nbhd ~ Bool, Eq nbhd, Finite nbhd)
 => (nbhd -> Bool -> [Bool]) -> Z3 s (FuncDecl s, FuncDecl s, nbhd -> Bool -> Z3 s (AST s))
setupRule rule = do
    (nbhdSort, nbhdFn) <- mkNbhdFn (fromCells @nbhd)
    boolSort <- mkBoolSort
    evol <- mkFreshFuncDecl "evolve" [nbhdSort, boolSort] boolSort
    let evolve1 :: nbhd -> Bool -> Z3 s (AST s)
        evolve1 nbhd cell =
            mkApp evol =<< sequenceA [mkInt (fi $ universalIndex nbhd) nbhdSort, mkBool cell]
    [(nbhdFn, evol, evolve1)
       | () <- for_ (universeF :: [(Bool, nbhd)]) $ \ (cell, nbhd) ->
               assert <=< mkOr <=< for (toList $ rule nbhd cell) $
               bind2 mkEq (evolve1 nbhd cell) . mkBool]

setupGrid :: ∀ nbhd i s .
    (Applicative (Shape nbhd), Traversable (Shape nbhd), Neighborly nbhd, Index nbhd ~ (i, i), Num i, Ix i)
 => Proxy nbhd -> FuncDecl s -> FuncDecl s -> Natural -> Array (i, i) (AST s) -> Z3 s (NonEmpty (Array (i, i) (AST s)))
setupGrid prox evol nbhdFn period = iterateM period evolve
  where
    evolve = evolve' (Pair <$> Identity <*> shape prox) $ \ (Pair (Identity a) as) ->
             mkApp evol =<< sequenceA [mkApp nbhdFn (toList as), pure a]

mkNbhdFn :: ∀ nbhd f s . (Applicative f, Traversable f, Eq nbhd, Finite nbhd) => (f Bool -> nbhd) -> Z3 s (Sort s, FuncDecl s)
mkNbhdFn f =
    [(nbhdSort, nbhdFn)
       | boolSort <- mkBoolSort
       , nbhdSortSymbol <- mkStringSymbol "Nbhd"
       , nbhdSort <- mkFiniteDomainSort nbhdSortSymbol (fi $ length (universeF :: [nbhd]))
       , nbhdFn <- mkFreshFuncDecl "nbhd" (toList $ pure @f boolSort) nbhdSort
       , () <- for_ (sequenceA $ pure (universeF :: [Bool])) $
               assert <=< bind2 mkEq <$> flip mkInt nbhdSort . fi . universalIndex . f
                                     <*> (mkApp nbhdFn . toList <=< traverse mkBool)]

fi = fromIntegral

mkArraysEqual :: (Ix i) => Array i (AST s) -> Array i (AST s) -> Z3 s (AST s)
mkArraysEqual a b = mkAnd . toList =<<
                    zipArraysA (curry $ \ case (Nothing, Nothing) -> mkBool True
                                               (Just x,  Nothing) -> mkNot x
                                               (Nothing, Just y)  -> mkNot y
                                               (Just x,  Just y)  -> mkEq x y) a b

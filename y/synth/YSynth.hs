{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module YSynth (Parms (..), go) where

import Prelude hiding (filter, head, last, replicate, id, (.))
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Bool
import Data.Foldable
import Data.Function (on)
import Data.Functor.Compose
import Data.Functor.Product
import Data.List.NonEmpty (NonEmpty (..), head)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Traversable
import Data.Universe.Class
import Data.Universe.Instances.Base ()
import Numeric.Natural
import Util
import Util.Array
import Util.Monad.Primitive.Unsafe
import Util.Universe
import Z3.Tagged

import GHC.Exts (IsList)
import qualified GHC.Exts as List (IsList (..))

import Nbhd
import YCommon

data Parms = Parms { goal :: Array (Int, Int) (Maybe Bool), reagents :: [Array (Int, Int) Bool], period :: Natural }
  deriving (Eq, Read, Show)

go :: ∀ nbhd .
      (Applicative (Shape nbhd), Traversable (Shape nbhd), Neighborly nbhd, Index nbhd ~ (Int, Int), Cell nbhd ~ Bool, Eq nbhd, Finite nbhd)
   => (nbhd -> Bool -> [Bool]) -> Parms -> [(Array (Int, Int) Bool, nbhd -> Bool -> Bool)]
go rule parms = fmap (head *** id) $ runST $ evalZ3 $ do
    (grids, evolve1) <- setup rule parms
    unsafeInterleaveWhileJust
        (runMaybeT
         [(a, curry b)
            | model <- MaybeT (snd <$> solverCheckAndGetModel)
            , rule <- lift $ for id (uncurry evolve1)
            , Pair (Compose a) (Fn b) <-
                  (MaybeT . evalBool model) `traverse` Pair (Compose grids) rule])
        (assert <=< (\ (a, b) -> mkOr [a, b]) <=<
         mkExcludeGrids grids . head *=*
         (mkExcludeSame <=< for (Fn id) . \ rule ->
          fmap . (,) . uncurry rule <*> uncurry evolve1))

setup :: ∀ nbhd s .
         (Applicative (Shape nbhd), Traversable (Shape nbhd), Neighborly nbhd, Cell nbhd ~ Bool, Index nbhd ~ (Int, Int), Eq nbhd, Finite nbhd)
      => (nbhd -> Bool -> [Bool]) -> Parms -> Z3 s (NonEmpty (Array (Int, Int) (AST s)), nbhd -> Bool -> Z3 s (AST s))
setup rule (Parms {..}) = do
    (nbhdFn, evol, evolve1) <- setupRule rule
    boolSort <- mkBoolSort
    intSort <- mkIntSort
    false <- mkBool False
    reagents' <- for reagents $ \ reagent ->
        [\ (i, j) -> do
             let f ((i', j'), b) =
                     bind3 mkIte (mkAnd . getZipList =<< traverse2 mkEq [x, y] =<<
                                  traverse (flip mkInt intSort) [i-i', j-j'])
                                 (mkBool b) . pure
             b <- foldrM f false (assocs reagent)
             mkAnd [present, b]
           | present <- mkFreshConst "present" boolSort
           , x <- mkFreshConst "x" intSort
           , y <- mkFreshConst "y" intSort
           , () <- assert =<< mkImplies present =<< mkAnd =<<
                   sequenceA
                   [mkGe x =<< mkInt (((-) `on` fst . fst) (bounds goal) (bounds reagent)) intSort,
                    mkGe y =<< mkInt (((-) `on` snd . fst) (bounds goal) (bounds reagent)) intSort,
                    mkLe x =<< mkInt (((-) `on` fst . snd) (bounds goal) (bounds reagent)) intSort,
                    mkLe y =<< mkInt (((-) `on` snd . snd) (bounds goal) (bounds reagent)) intSort]]
    grids <- setupGrid (Proxy :: _ nbhd) evol nbhdFn period <=< fnArrayA (bounds goal) $ \ k -> do
        bs <- traverse ($ k) reagents'
        assert =<< mkAtMostOneHot bs
        mkOr bs
    [(grids, evolve1)
       | () <- assert <=< mkOr <=< for (toList grids) $ \ grid ->
               mkAnd . toList =<<
               zipArraysA (join & maybe (pure $ mkBool True) (bool mkNot pure) & (. fromMaybe false))
                          goal grid]

mkAtMostOneHot :: [AST s] -> Z3 s (AST s)
mkAtMostOneHot = mkNot <=< mkOr <=< go
  where
    go = \ case
        [] -> pure []
        a:as -> (++) <$> for as (\ b -> mkAnd [a, b]) <*> go as

instance IsList (ZipList a) where
    type Item (ZipList a) = a
    fromList = ZipList
    toList = getZipList

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module YFind (Parms (..), go) where

import Prelude hiding (filter, head, last, replicate, id, (.))
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Bool
import Data.Filtrable
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Product
import Data.List.NonEmpty (NonEmpty (..), head, last)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Traversable
import Data.Tuple (swap)
import Data.Universe.Class
import Data.Universe.Instances.Base ()
import Numeric.Factors
import Util
import Util.Array
import Util.Monad.Primitive.Unsafe
import Util.Universe
import Z3.Tagged

import Atomic
import Nbhd
import qualified Symmetry
import YCommon

import Debug.Trace

data Parms = Parms { speed :: ((Int, Word), Word), init :: Array (Int, Int) (Maybe Bool), symmetry :: Maybe Symmetry.Mode, strictPeriod :: Bool }
  deriving (Eq, Read, Show)

go :: ∀ nbhd .
      (Applicative (Shape nbhd), Traversable (Shape nbhd), Neighborly nbhd, Index nbhd ~ (Int, Int), Cell nbhd ~ Bool, Eq nbhd, Finite nbhd)
   => (nbhd -> Bool -> [Bool]) -> Parms -> [(Array (Int, Int) Bool, nbhd -> Bool -> Bool)]
go rule parms = fmap (head *** id) . filter (isAtomic (Pair <$> Identity <*> shape (Proxy :: _ nbhd)) . fst) $ runST $ evalZ3 $ do
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
setup rule (Parms { speed = ((dx, fi -> dy), fi -> period), .. }) = do
    (nbhdFn, evol, evolve1) <- setupRule rule
    grids@(grid:|_) <- setupGrid (Proxy :: _ nbhd) evol nbhdFn period =<<
                       traverseWithIx (\ i -> maybe (mkFreshConst ("cell" ++ show i) =<< mkBoolSort) mkBool) init
    let grid' = transform (last grids)
          where transform = case symmetry of
                    Just (Symmetry.Mode {glideReflect = True, axis}) -> reflect axis
                    _ -> id

    (grids, evolve1) <$ do
        false <- mkBool False
        assert =<< mkAnd . toList =<< zipArraysA (join & maybe (pure $ mkBool True) (bool mkNot pure) & (. fromMaybe false)) init grid
        assert =<< mkArraysEqual grid (shift (dx, dy) grid')
        () <- flip trace () <$> solverToString
        assert =<< (mkOr . toList . ixmap ((il, jl), (ih, jl)) id) grid'
        when strictPeriod
             (let c = foldr gcd (fi period) [dx, dy]
                  grids' = flip mapMaybe (factors c) $ \ n ->
                           shift (n * dx `div` c, n * dy `div` c) <$> NE.init grids !!? fi n
              in for_ grids' $ assert <=< mkNot <=< mkArraysEqual grid)
        case symmetry of
            Just (Symmetry.Mode {glideReflect = True}) -> pure ()
            Just (Symmetry.Mode {glideReflect = False, axis}) -> for_ grids $ \ grid ->
                assert =<< (mkArraysEqual <*> reflect axis) grid
            Nothing -> assert =<< (mkOr . altMap (toList . ixmap ((il, jl), (il, jh)) id)) grids
  where
    reflect :: (Ix i, Num i) => Symmetry.Axis -> Array (i, i) (AST s) -> Array (i, i) (AST s)
    reflect = \ case
        Symmetry.Ortho -> reflectOrtho
        Symmetry.Dia   -> bool reflectDia reflectDia' (dx < 0)
    reflectOrtho a = ixmap (bounds a) (\ (i, j) -> (il + ih - i, j)) a
      where ((il, _), (ih, _)) = bounds a
    reflectDia a = ixmap (swap *** swap $ bounds a) swap a
    reflectDia' = reflectOrtho . reflectDia . reflectOrtho

    ((il, jl), (ih, jh)) = bounds init

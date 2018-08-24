{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module YFind (Parms (..), go) where

import Prelude hiding (filter, head, last, replicate)
import Control.Arrow
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Bool
import Data.Filtrable
import Data.Foldable
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Product
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..), head, last)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy
import Data.Traversable
import Data.Tuple (swap)
import Data.Universe.Class
import Data.Universe.Instances.Base ()
import Util
import Util.Array
import Util.Monad.Primitive.Unsafe
import Z3.Tagged

import Atomic
import Evolve
import Nbhd
import qualified Symmetry

data Parms = Parms { speed :: ((Int, Word), Word), init :: Array (Int, Int) (Maybe Bool), symmetry :: Maybe Symmetry.Mode, strictPeriod :: Bool }
  deriving (Eq, Read, Show)

go :: ∀ nbhd .
      (Applicative (Shape nbhd), Traversable (Shape nbhd), Neighborly nbhd, Index nbhd ~ (Int, Int), Cell nbhd ~ Bool, Eq nbhd, Finite nbhd)
   => (nbhd -> Bool -> [Bool]) -> Parms -> [(Array (Int, Int) Bool, nbhd -> Bool -> Bool)]
go rule parms = fmap (head *** id) . filter (isAtomic (Pair <$> Identity <*> shape (Proxy :: _ nbhd)) . fst) . unsafeInlinePrim $ do
    env <- newEnv Nothing mempty
    let e :: ∀ a . _ a -> _ a
        e = flip evalZ3WithEnv env
    (grids, nbhdSort, evol) <- e $ setup rule parms
    unsafeInterleaveWhileJust (e . runMaybeT $ do
                                   model <- MaybeT (snd <$> solverCheckAndGetModel)
                                   (,) <$> (getCompose <$> MaybeT (mapEval evalBool model (Compose grids)))
                                       <*> [\ nbhd cell -> fromJust $ List.lookup (nbhd, cell) vs
                                              | vs <- for universe $ \ (nbhd, cell) ->
                                                    fmap ((,) (nbhd, cell)) . MaybeT $
                                                    evalBool model =<< mkApp evol =<< sequenceA [mkInt (fe' nbhd) nbhdSort, mkBool cell]])
                              (e <<< assert <=< (mkOr . \ (a, b) -> [a, b]) <=< mkExclude grids . head *=* mkExcludeRule nbhdSort evol)

mkExclude :: (Ix i) => NonEmpty (Array (i, i) (AST s)) -> Array (i, i) Bool -> Z3 s (AST s)
mkExclude grids answer =
    mkAnd <=< for (toList grids) $
    mkNot <=< mkAnd . elems <=<
    zipArraysA (\ (fromMaybe False -> value) ->
                maybe (mkBool False) pure >=> \ ast ->
                mkEq ast =<< mkBool value) answer

mkExcludeRule :: (Eq nbhd, Finite nbhd)
              => Sort s -> FuncDecl s -> (nbhd -> Bool -> Bool) -> Z3 s (AST s)
mkExcludeRule nbhdSort evol rule =
    mkNot <=< mkAnd <=< for universe $ \ (nbhd, cell) ->
    bool mkNot pure (rule nbhd cell) =<< mkApp evol =<< sequenceA [mkInt (fe' nbhd) nbhdSort, mkBool cell]

setup :: ∀ nbhd s .
         (Applicative (Shape nbhd), Traversable (Shape nbhd), Neighborly nbhd, Cell nbhd ~ Bool, Index nbhd ~ (Int, Int), Eq nbhd, Finite nbhd)
      => (nbhd -> Bool -> [Bool]) -> Parms -> Z3 s (NonEmpty (Array (Int, Int) (AST s)), Sort s, FuncDecl s)
setup rule (Parms { speed = ((dx, fi -> dy), fi -> period), .. }) = do
    let prox = Proxy :: Proxy nbhd

    (nbhdSort, nbhdFn) <- mkNbhdFn (fromCells @nbhd)
    let mkNbhd :: nbhd -> Z3 s (AST s)
        mkNbhd = flip mkInt nbhdSort . fe'

    evol <-
        [evol
           | boolSort <- mkBoolSort
           , evol <- mkFreshFuncDecl "evolve" [nbhdSort, boolSort] boolSort
           , () <- for_ (universeF :: [(Bool, nbhd)]) $ \ (cell, nbhd) ->
                   assert <=< mkOr <=< for (toList $ rule nbhd cell) $
                   bind2 mkEq (mkApp evol =<< sequenceA [mkNbhd nbhd, mkBool cell]) . mkBool]

    let evolve = evolve' (Pair <$> Identity <*> shape prox) $ \ (Pair (Identity a) as) ->
                 mkApp evol =<< sequenceA [mkApp nbhdFn (toList as), pure a]

    let ((il, jl), (ih, jh)) = bounds init

    grids@(grid:|_) <- iterateM period evolve <=< sequenceA $ listArray ((il, jl), (ih, jh)) . repeat $ mkFreshBoolVar "cell"
    let grid' = transform (last grids)
          where transform = case symmetry of
                    Just (Symmetry.Mode {glideReflect = True, axis}) -> reflect axis
                    _ -> id

    (grids, nbhdSort, evol) <$ do
        false <- mkBool False
        assert =<< mkAnd . toList =<< zipArraysA (join & maybe (pure $ mkBool True) (bool mkNot pure) & (. fromMaybe false)) init grid
        assert =<< mkArraysEqual grid (shift (dx, dy) grid')
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

mkNbhdFn :: ∀ nbhd f s . (Applicative f, Traversable f, Eq nbhd, Finite nbhd) => (f Bool -> nbhd) -> Z3 s (Sort s, FuncDecl s)
mkNbhdFn f =
    [(nbhdSort, nbhdFn)
       | boolSort <- mkBoolSort
       , nbhdSortSymbol <- mkStringSymbol "Nbhd"
       , nbhdSort <- mkFiniteDomainSort nbhdSortSymbol (fi $ length (universeF :: [nbhd]))
       , nbhdFnSymbol <- mkStringSymbol "nbhd"
       , nbhdFn <- mkFuncDecl nbhdFnSymbol (toList $ pure @f boolSort) nbhdSort
       , () <- for_ (sequenceA $ pure (universeF :: [Bool])) $
               assert <=< bind2 mkEq <$> flip mkInt nbhdSort . fe' . f
                                     <*> (mkApp nbhdFn . toList <=< traverse mkBool)]

shift :: (Ix i, Num i) => (i, i) -> Array (i, i) a -> Array (i, i) a
shift (dx, dy) a = listArray ((il + dx, jl + dy), (ih + dx, jh + dy)) (elems a)
  where ((il, jl), (ih, jh)) = bounds a

fi = fromIntegral

mkArraysEqual :: (Ix i) => Array i (AST s) -> Array i (AST s) -> Z3 s (AST s)
mkArraysEqual a b = mkAnd . toList =<<
                    zipArraysA (curry $ \ case (Nothing, Nothing) -> mkBool True
                                               (Just x,  Nothing) -> mkNot x
                                               (Nothing, Just y)  -> mkNot y
                                               (Just x,  Just y)  -> mkEq x y) a b

factors :: (Integral a) => a -> [a]
factors n = filter ((==) 0 . mod n) [1..n]

fe' :: (Eq a, Universe a) => a -> Int
fe' = fromJust . flip List.findIndex universe . (==)

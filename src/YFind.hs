{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module YFind (Parms (..), go) where

import Prelude hiding (last, replicate)
import Control.Arrow
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Bool
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..), last)
import Data.Maybe
import Data.Tuple (swap)
import Util
import Util.Array
import Util.Monad.Primitive.Unsafe
import Z3.Tagged

import Evolve
import Rule
import qualified Symmetry

data Parms = Parms { speed :: ((Int, Word), Word), size :: (Word, Word), symmetry :: Maybe Symmetry.Mode }
  deriving (Eq, Read, Show)

go :: (Applicative f, Traversable f) => Rule (Int, Int) f Bool -> Parms -> [Array (Int, Int) Bool]
go rule parms = unsafeInlinePrim $ do
    env <- newEnv Nothing mempty
    let e = flip evalZ3WithEnv env
    grids@(grid:|_) <- e $ setup rule parms
    unsafeInterleaveWhileJust (e . runMaybeT $ getBoolValues grid) $ e . exclude grids

exclude :: (Ix i) => NonEmpty (Array (i, i) (AST s)) -> Array (i, i) Bool -> Z3 s ()
exclude grids answer =
    for_ grids (assert <=< mkNot <=< mkAnd . elems <=<
                zipArraysA (\ (fromMaybe False -> value) ->
                            maybe (mkBool False) pure >=> \ ast ->
                            mkEq ast =<< mkBool value) answer)

setup :: (Applicative f, Traversable f) => Rule (Int, Int) f Bool -> Parms -> Z3 s (NonEmpty (Array (Int, Int) (AST s)))
setup rule (Parms { speed = ((dx, fi -> dy), fi -> period)
                  , size = (fi -> width, fi -> height)
                  , symmetry }) = do
    grids@(grid:|_) <- iterateM period (evolve rule) <=< sequenceA $ listArray ((0, 0), (width-1, height-1)) . repeat $ mkFreshBoolVar "cell"
    let grid' = transform (last grids)
          where transform = case symmetry of
                    Just (Symmetry.Mode {glideReflect = True, axis}) -> reflect axis
                    _ -> id

    grids <$ do assert =<< mkArraysEqual grid (shift (dx, dy) grid')
                assert =<< (mkOr . toList . ixmap ((0, 0), (fst . snd $ bounds grid', 0)) id) grid'
                case symmetry of
                    Just (Symmetry.Mode {glideReflect = False, axis}) -> for_ grids $ \ grid ->
                        assert =<< (mkArraysEqual <*> reflect axis) grid
                    _ -> pure ()
  where
    reflect :: (Ix i, Num i) => Symmetry.Axis -> Array (i, i) (AST s) -> Array (i, i) (AST s)
    reflect = \ case
        Symmetry.Ortho -> reflectOrtho
        Symmetry.Dia   -> bool reflectDia reflectDia' (dx < 0)
    reflectOrtho a = ixmap (bounds a) (\ (i, j) -> (il + ih - i, j)) a
      where ((il, _), (ih, _)) = bounds a
    reflectDia a = ixmap (swap *** swap $ bounds a) swap a
    reflectDia' = reflectOrtho . reflectDia . reflectOrtho

shift :: (Ix i, Num i) => (i, i) -> Array (i, i) a -> Array (i, i) a
shift (dx, dy) a = listArray ((il + dx, jl + dy), (ih + dx, jh + dy)) (elems a)
  where ((il, jl), (ih, jh)) = bounds a

getBoolValues :: (Traversable f) => f (AST s) -> MaybeT (Z3 s) (f Bool)
getBoolValues xs = do
    model <- MaybeT (snd <$> solverCheckAndGetModel)
    MaybeT (mapEval evalBool model xs)

fi = fromIntegral

mkArraysEqual :: (Ix i) => Array i (AST s) -> Array i (AST s) -> Z3 s (AST s)
mkArraysEqual a b = mkAnd . toList =<<
                    zipArraysA (curry $ \ case (Nothing, Nothing) -> mkBool True
                                               (Just x,  Nothing) -> mkNot x
                                               (Nothing, Just y)  -> mkNot y
                                               (Just x,  Just y)  -> mkEq x y) a b

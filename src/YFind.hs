{-# LANGUAGE ViewPatterns #-}

module YFind (Parms (..), go) where

import Prelude hiding (last, replicate)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..), last)
import Data.Maybe
import Util
import Util.Array
import Util.Monad.Primitive.Unsafe
import Z3.Tagged

import Evolve
import Rule

data Parms = Parms { speed :: ((Int, Word), Word), size :: (Word, Word) }
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
                  , size = (fi -> width, fi -> height) }) = do
    grids@(grid:|_) <- iterateM period (evolve rule) <=< sequenceA $ listArray ((0, 0), (width-1, height-1)) . repeat $ mkFreshBoolVar "cell"
    let grid' = last grids

    grids <$ do assert =<< mkArraysEqual grid (shift (dx, dy) grid')
                assert =<< (mkOr . toList . ixmap ((0, 0), (width-1, 0)) id) grid'

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

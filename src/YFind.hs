{-# LANGUAGE ViewPatterns #-}

module YFind (Parms (..), go) where

import Prelude hiding (last, replicate)
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..), last, (<|))
import Data.Rule.Hex
import Numeric.Natural
import System.IO.Unsafe
import Util.Array
import Z3.Monad

import Evolve

data Parms = Parms { speed :: ((Int, Word), Word), size :: (Word, Word) }
  deriving (Eq, Read, Show)

go :: Rule -> Parms -> [Array (Int, Int) Bool]
go rule parms = unsafePerformIO $ do
    env <- newEnv Nothing mempty
    let e = flip evalZ3WithEnv env
    grids@(grid:|_) <- e $ setup rule parms
    unsafeInterleaveWhileJustIO (mapMaybeT e $ getBoolValues grid) $ e . exclude grids

exclude :: MonadZ3 z3 => NonEmpty (Array (Int, Int) AST) -> Array (Int, Int) Bool -> z3 ()
exclude grids answer =
    for_ grids (assert <=< mkNot <=< mkAnd . elems <=<
                zipArraysA (\ value ast -> mkEq ast =<< mkBool value) answer)

setup :: MonadZ3 z3 => Rule -> Parms -> z3 (NonEmpty (Array (Int, Int) AST))
setup rule (Parms { speed = ((dx, fi -> dy), fi -> period)
                  , size = (fi -> width, fi -> height) }) = do
    grids@(grid:|_) <- iterateM period (evolve rule) <=< sequenceA $ listArray ((0, 0), (width-1, height-1)) . repeat $ mkFreshBoolVar "cell"
    let grid' = last grids

    grids <$ do assert =<< mkArraysEqual grid (shift (dx, dy) grid')
                assert =<< (mkOr . toList . ixmap ((0, 0), (width-1, 0)) id) grid'

  where shift (dx, dy) a = ixmap ((il + dx, jl + dy), (ih + dx, jh + dy)) ((+ negate dx) *** (+ negate dy)) a
          where ((il, jl), (ih, jh)) = bounds a

getBoolValues :: (MonadZ3 z3, Traversable f) => f AST -> MaybeT z3 (f Bool)
getBoolValues xs = do
    model <- MaybeT (snd <$> solverCheckAndGetModel)
    MaybeT (mapEval evalBool model xs)

iterateM :: Monad m => Natural -> (a -> m a) -> a -> m (NonEmpty a)
iterateM 0 _ x = pure (x:|[])
iterateM k f x = (x <|) <$> (f x >>= iterateM (k-1) f)

fi = fromIntegral

unsafeInterleaveWhileJustIO :: MaybeT IO a -> (a -> IO ()) -> IO [a]
unsafeInterleaveWhileJustIO (MaybeT mma) f = go
  where go = mma >>= unsafeInterleaveIO . \ case
            Nothing -> pure []
            Just a -> (a :) <$> unsafeInterleaveIO (a `seq` f a >> unsafeInterleaveWhileJustIO (MaybeT mma) f)

mkArraysEqual :: (Ix i, MonadZ3 z3) => Array i AST -> Array i AST -> z3 AST
mkArraysEqual a b = mkAnd . toList =<<
                    zipArraysA' (curry $ \ case (Nothing, Nothing) -> mkBool True
                                                (Just x,  Nothing) -> mkNot x
                                                (Nothing, Just y)  -> mkNot y
                                                (Just x,  Just y)  -> mkEq x y) a b

{-# LANGUAGE ViewPatterns #-}

module YFind (Parms (..), go) where

import Prelude hiding (last, replicate)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Bool
import Data.Foldable
import Data.Rule.Hex
import Numeric.Natural
import System.IO.Unsafe
import Util
import Z3.Monad

import Evolve

data Parms = Parms { speed :: ((Int, Word), Word), size :: (Word, Word) }
  deriving (Eq, Read, Show)

go :: Rule -> Parms -> Maybe [Natural]
go rule (Parms { speed = ((dx, fi -> dy), fi -> period)
                 , size = (fi -> width, fi -> height) }) = unsafePerformIO . evalZ3 $ do
    zeroRow <- mkBitvector (fi width) 0
    rows <- replicateA height $ mkFreshBvVar "row" (fi width)
    rows' <- (iterateM period $ evolveRows rule) rows

    (bind2 . zipWithM_) (curry $ assert <=< uncurry mkEq)
        (traverse (pad dx)          (rows ++ replicate dy zeroRow))
        (traverse (pad (negate dx)) (replicate dy zeroRow ++ rows'))
    assert =<< mkNot =<< mkEq zeroRow =<< foldrM mkBvor zeroRow rows
    runMaybeT $ do
        model <- MaybeT (snd <$> solverCheckAndGetModel)
        fmap fi <$> MaybeT (mapEval evalInt model rows)

pad :: MonadZ3 z3 => Int -> AST -> z3 AST
pad 0 row = pure row
pad n row = bool flip id (n < 0) mkConcat row =<< mkBitvector (abs n) 0

iterateM :: Monad m => Natural -> (a -> m a) -> a -> m a
iterateM 0 _ = pure
iterateM k f = f >=> iterateM (k-1) f

fi = fromIntegral

{-# LANGUAGE ViewPatterns #-}

module YFind where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Rule.Hex
import Numeric.Natural
import System.IO.Unsafe
import Util
import Z3.Monad

import Evolve

data StillParms = StillParms { size :: (Word, Word) }

findStill :: Rule -> StillParms -> Maybe [Natural]
findStill rule (StillParms { size = (width, fi -> height) }) = unsafePerformIO . evalZ3 $ do
    zeroRow <- mkBitvector (fi width) 0
    rows <- replicateA height $ mkFreshBvVar "row" (fi width)
    zipWithM_ (curry $ assert <=< uncurry mkEq) rows =<< evolveRows rule rows
    assert =<< mkNot =<< mkEq zeroRow =<< foldrM mkBvor zeroRow rows
    runMaybeT $ do
        model <- MaybeT (snd <$> solverCheckAndGetModel)
        fmap fi <$> MaybeT (mapEval evalInt model rows)

fi = fromIntegral

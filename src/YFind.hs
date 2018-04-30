{-# LANGUAGE ViewPatterns #-}

module YFind (Parms (..), go) where

import Prelude hiding (head, last, replicate)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Bool
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..), head, last, (<|))
import Data.Rule.Hex
import Numeric.Natural
import System.IO.Unsafe
import Util
import Z3.Monad

import Evolve

data Parms = Parms { speed :: ((Int, Word), Word), size :: (Word, Word) }
  deriving (Eq, Read, Show)

go :: Rule -> Parms -> [[Natural]]
go rule parms = unsafePerformIO $ do
    env <- newEnv Nothing mempty
    let e = flip evalZ3WithEnv env
    rowses@(rows:|_) <- e $ setup rule parms
    unsafeInterleaveWhileJustIO (mapMaybeT e $ getRows rows) $ e . exclude rowses

exclude :: MonadZ3 z3 => NonEmpty [AST] -> [Natural] -> z3 ()
exclude rowses answer =
    for_ rowses (assert <=< mkNot <=< mkAnd <=<
                 zipWithM (\ value ast ->
                           mkEq ast =<< mkIntegral value =<< getSort ast) answer)

setup :: MonadZ3 z3 => Rule -> Parms -> z3 (NonEmpty [AST])
setup rule (Parms { speed = ((dx, fi -> dy), fi -> period)
                 , size = (fi -> width, fi -> height) }) = do
    zeroRow <- mkBitvector (fi width) 0
    rowses <- replicateA height (mkFreshBvVar "row" (fi width)) >>= iterateM period (evolveRows rule)
    let rows  = head rowses
        rows' = last rowses

    (bind2 . zipWithM_) (curry $ assert <=< uncurry mkEq)
        (traverse (pad dx)          (rows ++ replicate dy zeroRow))
        (traverse (pad (negate dx)) (replicate dy zeroRow ++ rows'))

    rowses <$ case rows' of row:_ -> assert =<< mkNot =<< mkEq zeroRow row
                            _     -> assert =<< mkBool False

getRows :: MonadZ3 z3 => [AST] -> MaybeT z3 [Natural]
getRows rows = do
    model <- MaybeT (snd <$> solverCheckAndGetModel)
    fmap fi <$> MaybeT (mapEval evalInt model rows)

pad :: MonadZ3 z3 => Int -> AST -> z3 AST
pad 0 row = pure row
pad n row = bool flip id (n < 0) mkConcat row =<< mkBitvector (abs n) 0

iterateM :: Monad m => Natural -> (a -> m a) -> a -> m (NonEmpty a)
iterateM 0 _ x = pure (x:|[])
iterateM k f x = (x <|) <$> (f x >>= iterateM (k-1) f)

fi = fromIntegral

unsafeInterleaveWhileJustIO :: MaybeT IO a -> (a -> IO ()) -> IO [a]
unsafeInterleaveWhileJustIO (MaybeT mma) f = go
  where go = mma >>= unsafeInterleaveIO . \ case
            Nothing -> pure []
            Just a -> (a :) <$> unsafeInterleaveIO (a `seq` f a >> unsafeInterleaveWhileJustIO (MaybeT mma) f)

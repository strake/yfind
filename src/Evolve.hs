module Evolve where

import Prelude hiding (replicate)
import Control.Monad
import Data.Bits
import Data.Bool
import Data.Rule.Hex
import Data.Traversable
import Numeric.Natural
import Util
import Z3.Monad

mkTestBit :: MonadZ3 z3 => Int -> AST -> z3 AST
mkTestBit k = bind2 mkEq (mkBitvector 1 1) . mkExtract k k

evolveRow :: MonadZ3 z3 => Rule -> AST -> AST -> AST -> AST -> z3 ()
evolveRow rule = \ x a b c -> do
    width <- (getSort >=> getBvSortSize) x
    [a, b, c] <- traverse (pad 2) [a, b, c]
    x <- pad 1 x
    forM_ [0..width+1] $ \ k -> do
        nbhd <- foldr1 (bind2 mkConcat)
                [mkExtract (k+1) (k+0) c, mkExtract (k+2) (k+0) b, mkExtract (k+2) (k+1) a]
        assert =<< bind2 mkEq (mkTestBit k x) (mkOr =<< [0..127] `for` \ k -> bool (mkBool False) (mkEq nbhd =<< mkBitvector 7 k) $ testBit (tabulate rule) (fromIntegral k))
  where pad n a = foldr1 (bind2 mkConcat)
                  [mkBitvector n 0, pure a, mkBitvector n 0]

evolveRows :: MonadZ3 z3 => Rule -> [AST] -> z3 [AST]
evolveRows rule rows = do
    let pad :: MonadZ3 z3 => Natural -> [AST] -> z3 [AST]
        pad _ [] = pure []
        pad n xs@(x:_) =
            (\ zero -> replicate n zero ++ xs ++ replicate n zero) <$> (getSort >=> getBvSortSize >=> flip mkBitvector 0) x
    rows' <- traverse (getSort >=> mkFreshConst "row") rows
    rows' <$ bind2 (zipWithM_ (\ x [a, b, c] -> evolveRow rule x a b c)) (pad 1 rows') ((fmap (windows 3) . pad 2) rows)

windows :: Int -> [a] -> [[a]]
windows n xs | length xs >= n, _:xs' <- xs = take n xs : windows n xs'
windows _ _ = []

{-# LANGUAGE PackageImports #-}

module Util (module A, factors, universalIndex) where

import qualified Data.List as List
import Data.Universe.Class
import Numeric.Natural

import "util" Util as A

factors :: (Integral a) => a -> [a]
factors n = filter ((==) 0 . mod n) [1..n]

universalIndex :: (Eq a, Universe a) => a -> Natural
universalIndex a = List.genericLength $ List.takeWhile (/= a) universe

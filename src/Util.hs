{-# LANGUAGE PackageImports #-}

module Util (module A, factors, universalIndex, Fn (..)) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad
import Data.Functor.Compose
import qualified Data.List as List
import Data.Maybe
import Data.Universe.Class
import Numeric.Natural

import "util" Util as A

factors :: (Integral a) => a -> [a]
factors n = filter ((==) 0 . mod n) [1..n]

universalIndex :: (Eq a, Universe a) => a -> Natural
universalIndex a = List.genericLength $ List.takeWhile (/= a) universe

newtype Fn a b = Fn { unFn :: a -> b } deriving (Functor, Applicative, Monad, Category)
instance Universe a => Foldable (Fn a) where
    foldMap f (Fn φ) = foldMap (f . φ) universe
instance (Eq a, Finite a) => Traversable (Fn a) where
    sequenceA (Fn φ) =
        Fn . (\ bs a -> fromJust $ List.lookup a bs) . getCompose <$>
        traverse φ (Compose $ join (,) <$> universe)

module Util.Array where

import Control.Monad (guard)
import Data.Array

(!?) :: Ix i => Array i a -> i -> Maybe a
as !? i = as ! i <$ guard (inRange (bounds as) i)

zipArraysA :: (Ix i, Applicative p) => (a -> b -> p c) -> Array i a -> Array i b -> p (Array i c)
zipArraysA f as bs = sequenceA $ array (l, h) [(i, f (as ! i) (bs ! i)) | i <- range (l, h)]
  where (al, ah) = bounds as
        (bl, bh) = bounds bs
        (l, h) = (max al bl, min ah bh)

zipArraysA' :: (Ix i, Applicative p) => (Maybe a -> Maybe b -> p c) -> Array i a -> Array i b -> p (Array i c)
zipArraysA' f as bs = sequenceA $ array (l, h) [(i, f (as !? i) (bs !? i)) | i <- range (l, h)]
  where (al, ah) = bounds as
        (bl, bh) = bounds bs
        (l, h) = (min al bl, max ah bh)

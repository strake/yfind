module Util.Array where

import Control.Monad (guard)
import Data.Array
import Data.Traversable

(!?) :: Ix i => Array i a -> i -> Maybe a
as !? i = as ! i <$ guard (inRange (bounds as) i)

zipArraysA :: (Ix i, Applicative p) => (Maybe a -> Maybe b -> p c) -> Array i a -> Array i b -> p (Array i c)
zipArraysA f as bs = fnArrayA (min al bl, max ah bh) $ \ i -> f (as !? i) (bs !? i)
  where (al, ah) = bounds as
        (bl, bh) = bounds bs

shift :: (Ix i, Num i) => (i, i) -> Array (i, i) a -> Array (i, i) a
shift (dx, dy) a = listArray ((il + dx, jl + dy), (ih + dx, jh + dy)) (elems a)
  where ((il, jl), (ih, jh)) = bounds a

fnArrayA :: (Ix i, Applicative p) => (i, i) -> (i -> p a) -> p (Array i a)
fnArrayA = (.) <$> fmap . listArray <*> for . range

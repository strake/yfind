module Numeric.Factors where

factors :: (Integral a) => a -> [a]
factors n = filter ((==) 0 . mod n) [1..n]

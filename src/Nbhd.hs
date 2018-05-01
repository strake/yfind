module Nbhd where

type Nbhd i = i -> [i]

rawHexNbhd :: (Num i, Num j) => Nbhd (i, j)
rawHexNbhd (i, j) = [(i+0, j+1),
                     (i-1, j+1),
                     (i+1, j+0),
                     (i+0, j+0),
                     (i-1, j+0),
                     (i+1, j-1),
                     (i+0, j-1)]

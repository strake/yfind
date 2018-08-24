{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Nbhd where

import Data.Fin.List
import Data.Maybe (fromJust)
import Data.Peano
import Data.Proxy
import qualified Data.Neighborhood.Hex as Hex
import qualified Data.Neighborhood.Moore as Moore

class Neighborly nbhd where
    type Index nbhd :: *
    type Shape nbhd :: * -> *
    type Cell nbhd :: *
    shape :: Proxy nbhd -> Index nbhd -> Shape nbhd (Index nbhd)
    fromCells :: Shape nbhd (Cell nbhd) -> nbhd

instance Neighborly Hex.Nbhd where
    type Index Hex.Nbhd = (Int, Int)
    type Shape Hex.Nbhd = List (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))
    type Cell Hex.Nbhd = Bool
    shape _ = fromJust . fromList . \ (i, j) ->
        [(i+0, j+1), (i-1, j+1), (i-1, j+0), (i+0, j-1), (i+1, j-1), (i+1, j+0)]
    fromCells = Hex.fromList

instance Neighborly Moore.Nbhd where
    type Index Moore.Nbhd = (Int, Int)
    type Shape Moore.Nbhd = List (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))
    type Cell Moore.Nbhd = Bool
    shape _ = fromJust . fromList . \ (i, j) ->
        [(i+1, j+1), (i+0, j+1), (i-1, j+1), (i-1, j+0), (i-1, j-1), (i+0, j-1), (i+1, j-1), (i+1, j+0)]
    fromCells = Moore.fromList

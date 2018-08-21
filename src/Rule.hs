{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Rule where

import Data.Bits
import Data.Bits.Bitwise
import Data.Fin.List
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Peano
import qualified Data.Rule.Hex as Hex
import qualified Data.Rule.Moore as Moore

data Rule i f a = Rule { nbhd :: i -> f i, evolveCell :: f a -> a }

class Regular rule where
    type Index rule :: *
    type Nbhd rule :: * -> *
    type Cell rule :: *
    toRule :: rule -> Rule (Index rule) (Nbhd rule) (Cell rule)

instance Regular Hex.Rule where
    type Index Hex.Rule = (Int, Int)
    type Nbhd Hex.Rule = List (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))
    type Cell Hex.Rule = Bool
    toRule rule = Rule { nbhd = fromJust . fromList . \ (i, j) ->
                             [            (i+0, j+1), (i-1, j+1),
                              (i+1, j+0), (i+0, j+0), (i-1, j+0),
                              (i+1, j-1), (i+0, j-1)            ]
                       , evolveCell = testBit table . fromListLE . toList }
      where table = Hex.tabulate rule

instance Regular Moore.Rule where
    type Index Moore.Rule = (Int, Int)
    type Nbhd Moore.Rule = List (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))
    type Cell Moore.Rule = Bool
    toRule rule = Rule { nbhd = fromJust . fromList . \ (i, j) ->
                             [(i+1, j+1), (i+0, j+1), (i-1, j+1),
                              (i+1, j+0), (i+0, j+0), (i-1, j+0),
                              (i+1, j-1), (i+0, j-1), (i-1, j-1)]
                       , evolveCell = testBit table . fromListLE . toList }
      where table = Moore.tabulate rule

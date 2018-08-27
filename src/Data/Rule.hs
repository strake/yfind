{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Rule where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Char
import Data.Foldable1
import Data.List (stripPrefix)
import qualified Data.Neighborhood.Hex as Hex
import qualified Data.Neighborhood.Moore as Moore
import qualified Data.Rule.Hex as Hex
import qualified Data.Rule.Moore as Moore
import Data.Typeable
import Data.Universe.Class
import Text.Read (readMaybe)
import Util
import Util.List

import Nbhd

data Rule' a where
    Rule' :: (Typeable nbhd, Neighborly nbhd, Index nbhd ~ (Int, Int), Cell nbhd ~ a, Applicative (Shape nbhd), Traversable (Shape nbhd), Eq nbhd, Finite nbhd)
          => (nbhd -> a -> [a]) -> Rule' a

orRule :: Typeable a => Rule' a -> Rule' a -> Maybe (Rule' a)
orRule (Rule' f) (Rule' g) = (\ g -> Rule' $ (liftA2 . liftA2) (<|>) f g) <$> cast g

parseRules' :: [Char] -> Maybe (Rule' Bool)
parseRules' = splitWhen (== ',') & traverse parseRule' >=> foldrM1 orRule

parseRule' :: [Char] -> Maybe (Rule' Bool)
parseRule' (fmap toUpper -> s)
  | Just s <- stripSuffix "H" s = Rule' . fromHexRule <$> readMaybe @Hex.Rule s
  | otherwise = Rule' . fromMooreRule <$> readMaybe @Moore.Rule s <|>
                Rule' . fromHexRule <$> readMaybe @Hex.Rule s

fromHexRule :: Hex.Rule -> Hex.Nbhd -> Bool -> [Bool]
fromHexRule rule nbhd a = [nbhd ∈ bool Hex.birth Hex.survival a rule]

fromMooreRule :: Moore.Rule -> Moore.Nbhd -> Bool -> [Bool]
fromMooreRule rule nbhd a = [nbhd ∈ bool Moore.birth Moore.survival a rule]

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix x = fmap reverse . stripPrefix (reverse x) . reverse

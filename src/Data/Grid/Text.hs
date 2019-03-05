{-# LANGUAGE ViewPatterns #-}

module Data.Grid.Text where

import Control.Monad
import Data.Array
import Data.Bool
import Data.Char
import Data.Foldable
import Data.Maybe
import Text.Read (readMaybe)
import Util
import Util.List

showGrid :: Array (Int, Int) Bool -> [Char]
showGrid a = flip concatMap [jl..jh] $ \ j ->
             (++ " $\n") . flip fmap (reverse [il..ih]) $ \ i ->
             bool '.' 'o' $ a ! (i, j)
  where ((il, jl), (ih, jh)) = bounds a

readGrid :: [Char] -> Maybe (Array (Int, Int) (Maybe Bool))
readGrid =
    (splitWhen (== '\n') &
     (traverse . traverse $ \ case
          '.' -> Just (Just False)
          'o' -> Just (Just True)
          ' ' -> Just Nothing
          _   -> Nothing)) &
    fmap (\ rows ->
          let (width, height) = (maximum $ length <$> rows, length rows)
          in array ((0, 0), (width-1, height-1))
             [((i, j), c) | (j, row) <- zip [0..] (toList rows)
                          , (i, c)   <- zip [0..] (reverse . take width $ row ++ repeat Nothing)])

readRle :: [Char] -> Maybe (Array (Int, Int) Bool)
readRle = fmap (\ case '$' -> '\n'; '!' -> '\n'; 'b' -> '.'; x -> x) & expandRle >=> (fmap . fmap) (fromMaybe False) . readGrid
  where
    expandRle = span isDigit & \ case
        ([], []) -> Just []
        ([], x:xs) -> (x:) <$> expandRle xs
        (readMaybe -> Just n, x:xs) -> (take n (repeat x) ++) <$> expandRle xs
        _ -> Nothing

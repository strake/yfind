module Data.Grid.Text where

import Data.Array
import Data.Bool
import Data.Foldable
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

{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Main where

import Control.Arrow
import Data.Array
import Data.Bool
import Data.List (elemIndex)
import Data.Rule.Hex
import Options.Applicative
import Text.Read (readMaybe)
import Util
import YFind

main :: IO ()
main = do
    Options {..} <- execParser $ info options mempty
    let Parms { size = (fromIntegral -> width, _) } = parms
    foldMapA (putStrLn . showGrid) $ go rule parms

data Options = Options { rule :: Rule, parms :: Parms }

options :: Parser Options
options = Options <$> option (maybeReader readMaybe) (short 'r' <> help "rule")
                  <*> (Parms <$> option (maybeReader $ \ s ->
                                         (read *** read . tail <<< flip splitAt s) <$> elemIndex '/' s)
                                        (short 'v' <> help "speed" <> value ((0,0),1))
                             <*> option auto (short 's' <> help "size"))

showGrid :: Array (Int, Int) Bool -> [Char]
showGrid a = flip concatMap [jl..jh] $ \ j ->
             (++ "\n") . flip fmap (reverse [il..ih]) $ \ i ->
             bool '.' 'o' $ a ! (i, j)
  where ((il, jl), (ih, jh)) = bounds a

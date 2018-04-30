{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Main where

import Control.Arrow
import Data.List (elemIndex, genericLength, genericTake)
import Data.Rule.Hex
import Numeric (showIntAtBase)
import Numeric.Natural
import Options.Applicative
import Text.Read (readMaybe)
import YFind

main :: IO ()
main = do
    Options {..} <- execParser $ info options mempty
    let Parms { size = (fromIntegral -> width, _) } = parms
    maybe (putStrLn "failed") (putStr . concatMap (showRow width)) $ go rule parms

data Options = Options { rule :: Rule, parms :: Parms }

options :: Parser Options
options = Options <$> option (maybeReader readMaybe) (short 'r' <> help "rule")
                  <*> (Parms <$> option (maybeReader $ \ s ->
                                         (read *** read . tail <<< flip splitAt s) <$> elemIndex '/' s)
                                        (short 'v' <> help "speed" <> value ((0,0),1))
                             <*> option auto (short 's' <> help "size"))

padLeft :: Natural -> a -> [a] -> [a]
padLeft n x xs | l >= n = xs
               | otherwise = genericTake (n - l) (repeat x) ++ xs
  where l = genericLength xs

showRow :: Natural -> Natural -> [Char]
showRow l = padLeft (l+1) '.' . ($ "\n") . showIntAtBase 2 (\ case 0 -> '.'; _ -> 'o')

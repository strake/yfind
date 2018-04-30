{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Main where

import Data.List (genericLength, genericTake)
import Data.Rule.Hex
import Numeric (showIntAtBase)
import Numeric.Natural
import Options.Applicative
import Text.Read (readMaybe)
import YFind

main :: IO ()
main = do
    Options {..} <- execParser $ info options mempty
    let StillParms { size = (fromIntegral -> width, _) } = stillParms
    maybe (putStrLn "failed") (putStr . concatMap (showRow width)) $ findStill rule stillParms

data Options = Options { rule :: Rule, stillParms :: StillParms }

options :: Parser Options
options = Options <$> option (maybeReader readMaybe) (short 'r' <> help "rule")
                  <*> (StillParms <$> option auto (short 's' <> help "size"))

padLeft :: Natural -> a -> [a] -> [a]
padLeft n x xs | l >= n = xs
               | otherwise = genericTake (n - l) (repeat x) ++ xs
  where l = genericLength xs

showRow :: Natural -> Natural -> [Char]
showRow l = padLeft (l+1) '.' . ($ "\n") . showIntAtBase 2 (\ case 0 -> '.'; _ -> 'o')

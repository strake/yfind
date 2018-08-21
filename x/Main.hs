{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow
import Data.Array
import Data.Bool
import Data.Char
import Data.List (elemIndex)
import qualified Data.Rule.Hex as Hex
import qualified Data.Rule.Moore as Moore
import Options.Applicative
import Text.Read (readMaybe)
import Util
import YFind

import Rule

main :: IO ()
main = do
    Options {..} <- execParser $ info options mempty
    foldMapA (putStrLn . showGrid) $ case rule' of Rule' rule -> go rule parms

data Options = Options { rule' :: Rule' (Int, Int) Bool, parms :: Parms }

data Rule' i a where Rule' :: (Applicative f, Traversable f) => Rule i f a -> Rule' i a

options :: Parser Options
options = Options <$> option (maybeReader $ \ s ->
                              case s of [] -> Nothing
                                        _:_ | 'H' <- toUpper (last s) -> Rule' . toRule <$> readMaybe @Hex.Rule (init s)
                                            | otherwise -> Rule' . toRule <$> readMaybe @Moore.Rule s <|>
                                                           Rule' . toRule <$> readMaybe @Hex.Rule   s) (short 'r' <> metavar "rule")
                  <*> (Parms <$> option (maybeReader $ \ s ->
                                         (read *** read . tail <<< flip splitAt s) <$> elemIndex '/' s)
                                        (short 'v' <> metavar "speed" <> value ((0,0),1))
                             <*> option auto (short 's' <> metavar "size"))

showGrid :: Array (Int, Int) Bool -> [Char]
showGrid a = flip concatMap [jl..jh] $ \ j ->
             (++ "\n") . flip fmap (reverse [il..ih]) $ \ i ->
             bool '.' 'o' $ a ! (i, j)
  where ((il, jl), (ih, jh)) = bounds a

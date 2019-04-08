{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Main where

import Control.Category.Dual
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader
import Data.Array
import Data.Foldable
import Data.Functor.Compose
import Data.Grid.Text
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Rule
import qualified Data.Rule.Hex as Hex
import qualified Data.Rule.Moore as Moore
import Data.Typeable
import Options.Applicative
import Text.Read (readMaybe)
import Util
import Util.Array

import qualified Symmetry
import Search

main :: IO ()
main = execParser (info options mempty) >>= \ Options { rule' = Rule' rule, .. } -> do
    let showHeader r a = asum ["x = ", show x, ", y = ", show y, ", ",
                               "rule = ", fromMaybe "?" $ showRule r, "\n"]
          where ((il, jl), (ih, jh)) = bounds a
                (x, y) = (ih - il + 1, jh - jl + 1)
        showRule = asumF [f Moore.fromFn, f Hex.fromFn]
          where f φ = fmap (intercalate "," . fmap (show . φ . dual) . getCompose) .
                      gcast . Compose . fmap Dual
    (inits, strictSize) <- flip fmap getContents $ readGrid & \ case
        Just init -> ([init], False)
        Nothing -> ([fnArray ((0, 0), (n, n)) (pure Nothing) | n <- [0..]], True)
    altMap (search rule . curry parms' strictSize) inits `for'` \ (grid, rule) -> putStrLn . asumF [showHeader rule, showGrid] $ grid
  where asumF = runReaderT . altMap ReaderT
        for' = flip foldMapA

data Options = Options { rule' :: Rule' Bool, parms' :: (Bool, Array (Int, Int) (Maybe Bool)) -> Parms }

options :: Parser Options
options =
    [Options {..}
       | rule' <- option (maybeReader parseRules') (short 'r' <> metavar "rule")
       , parms' <- getCompose
             [Parms {..}
                | init       <- Compose (pure snd)
                , strictSize <- Compose (pure fst)
                , speed <- lift $
                      option (maybeReader $
                              readMaybe *=* readMaybe . tail <=<
                              fmap <$> flip splitAt <*> elemIndex '/')
                             (short 'v' <> metavar "speed" <> value ((0,0),1))
                , symmetry <- lift $
                      option (maybeReader $ fmap Just . parseSymmetryMode)
                             (short 'g' <> metavar "symmetry" <> value Nothing)
                , strictPeriod          <- lift $ switch (long "strict-period")
                , idempotentRule        <- lift $ switch (long "rule-idempotent")
                , selfComplementaryRule <- lift $ switch (long "rule-self-complementary")]]
  where
    parseSymmetryMode :: [Char] -> Maybe Symmetry.Mode
    parseSymmetryMode s = [Symmetry.Mode {..}
                             | axis <- case s' of
                                   "ortho" -> Just Symmetry.Ortho
                                   "dia"   -> Just Symmetry.Dia
                                   _       -> Nothing]
      where (s', glideReflect) = case stripSuffix "~" s of
                Nothing -> (s, False)
                Just s' -> (s', True)

    lift = Compose . fmap pure

{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Category.Dual
import Control.Monad
import Control.Monad.Trans.MaybeReader
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
import Util

import qualified Symmetry
import YFind

main :: IO ()
main = do
    Options { rule' = Rule' rule, .. } <- execParser (info (unMaybeReaderT options) mempty) >>= \ case
        Left o -> pure o
        Right f -> f . fromMaybe (error "no parse") . readGrid <$> getContents
    let showHeader r a = asum ["x = ", show x, ", y = ", show y, ", ",
                               "rule = ", fromMaybe "?" $ showRule r, "\n"]
          where ((il, jl), (ih, jh)) = bounds a
                (x, y) = (ih - il + 1, jh - jl + 1)
        showRule = asumF [f Moore.fromFn, f Hex.fromFn]
          where f φ = fmap (intercalate "," . fmap (show . φ . dual) . getCompose) .
                      gcast . Compose . fmap Dual
    go rule parms `for'` \ (grid, rule) -> putStrLn . asumF [showHeader rule, showGrid] $ grid
  where asumF = runReaderT . altMap ReaderT
        for' = flip foldMapA

data Options = Options { rule' :: Rule' Bool, parms :: Parms }

options :: MaybeReaderT (Array (Int, Int) (Maybe Bool)) Parser Options
options = Options <$> lift (option (maybeReader parseRules') (short 'r' <> metavar "rule"))
                  <*> (Parms <$> lift (option (maybeReader $ \ s ->
                                               (read *** read . tail <<< flip splitAt s) <$> elemIndex '/' s)
                                              (short 'v' <> metavar "speed" <> value ((0,0),1)))
                             <*> ((\ size -> listArray ((0, 0), join (***) (+ negate 1) size) (repeat Nothing)) <$>
                                  lift (option auto (short 's' <> metavar "size")) <|>
                                  lift (flag' (Right ()) (short 'i')) *> MaybeReaderT (pure $ Right id))
                             <*> lift (option (maybeReader $ fmap Just . parseSymmetryMode)
                                              (short 'g' <> metavar "symmetry" <> value Nothing))
                             <*> lift (switch (long "strict-period"))
                             <*> lift (switch (long "rule-idempotent"))
                             <*> lift (switch (long "rule-self-complementary")))
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

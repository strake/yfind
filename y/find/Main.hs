{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.MaybeReader
import Control.Monad.Trans.Reader
import Data.Array
import Data.Foldable
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
    Options {..} <- execParser (info (unMaybeReaderT options) mempty) >>= \ case
        Left o -> pure o
        Right f -> f . fromMaybe (error "no parse") . readGrid <$> getContents
    case rule' of
        Rule' rule ->
            let showHeader r a = asum ["x = ", show x, ", y = ", show y, ", rule = ", fromMaybe "?" $ showRule r, "\n"]
                  where ((il, jl), (ih, jh)) = bounds a
                        (x, y) = (ih - il + 1, jh - jl + 1)
                showRule = asum . sequenceA [fmap (show . Moore.fromFn) . cast,
                                             fmap (show . Hex.fromFn)   . cast]
            in foldMapA (\ (grid, rule) ->
                         putStrLn . runReaderT (altMap ReaderT [showHeader rule, showGrid]) $ grid) $ go rule parms

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

{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Main where

import Control.Applicative
import Control.Monad.Trans.Reader
import Data.Array
import Data.Foldable
import Data.Functor.Compose
import Data.Grid.Text
import Data.Maybe (fromMaybe)
import Data.Rule
import qualified Data.Rule.Hex as Hex
import qualified Data.Rule.Moore as Moore
import Data.Typeable
import Options.Applicative
import Util

import YSynth

import Debug.Trace

main :: IO ()
main = do
    Options {..} <- execParser (info options mempty) <*> (getContents >>= maybe (error "no parse") pure . readGrid)
    do
        let Parms {..} = parms
        for_ reagents $ \ reagent -> trace (showGrid reagent) $ pure ()
        trace (showGrid (fromMaybe False <$> goal)) $ pure ()
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

options :: Parser (Array (Int, Int) (Maybe Bool) -> Options)
options = getCompose $
    Options <$> (Compose . fmap pure $
                 option (maybeReader parseRules') (short 'r' <> metavar "rule"))
            <*> (Compose $
                 (\ period reagents goal -> Parms {..}) <$>
                 option auto (short 'p' <> metavar "period") <*>
                 many (argument (maybeReader readRle) (metavar "reagent")))

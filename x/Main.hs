{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE GADTs, TypeFamilies, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Main where

import Prelude hiding (maximum)

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Array
import Data.Bool
import Data.Char
import Data.Foldable hiding (maximum)
import Data.Foldable1
import Data.List (elemIndex, reverse, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import qualified Data.Neighborhood.Hex as Hex
import qualified Data.Neighborhood.Moore as Moore
import qualified Data.Rule.Hex as Hex
import qualified Data.Rule.Moore as Moore
import Data.Typeable
import Data.Universe.Class
import Options.Applicative
import Text.Read (readMaybe)
import Util

import Nbhd
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

data Rule' a where
    Rule' :: (Typeable nbhd, Neighborly nbhd, Index nbhd ~ (Int, Int), Cell nbhd ~ a, Applicative (Shape nbhd), Traversable (Shape nbhd), Eq nbhd, Finite nbhd)
          => (nbhd -> a -> [a]) -> Rule' a

orRule :: Typeable a => Rule' a -> Rule' a -> Maybe (Rule' a)
orRule (Rule' f) (Rule' g) = (\ g -> Rule' $ (liftA2 . liftA2) (<|>) f g) <$> cast g

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
                             <*> lift (switch (long "strict-period")))
  where
    parseRules' :: [Char] -> Maybe (Rule' Bool)
    parseRules' = splitWhen (== ',') & traverse parseRule' >=> foldrM1 orRule

    parseRule' :: [Char] -> Maybe (Rule' Bool)
    parseRule' (fmap toUpper -> s)
      | Just s <- stripSuffix "H" s = Rule' . fromHexRule <$> readMaybe @Hex.Rule s
      | otherwise = Rule' . fromMooreRule <$> readMaybe @Moore.Rule s <|>
                    Rule' . fromHexRule <$> readMaybe @Hex.Rule s

    fromHexRule :: Hex.Rule -> Hex.Nbhd -> Bool -> [Bool]
    fromHexRule rule nbhd a = [nbhd ∈ bool Hex.birth Hex.survival a rule]

    fromMooreRule :: Moore.Rule -> Moore.Nbhd -> Bool -> [Bool]
    fromMooreRule rule nbhd a = [nbhd ∈ bool Moore.birth Moore.survival a rule]

    parseSymmetryMode :: [Char] -> Maybe Symmetry.Mode
    parseSymmetryMode s = [Symmetry.Mode {..}
                             | axis <- case s' of
                                   "ortho" -> Just Symmetry.Ortho
                                   "dia"   -> Just Symmetry.Dia
                                   _       -> Nothing]
      where (s', glideReflect) = case stripSuffix "~" s of
                Nothing -> (s, False)
                Just s' -> (s', True)

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

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix x = fmap reverse . stripPrefix (reverse x) . reverse

newtype MaybeReaderT r m a = MaybeReaderT { unMaybeReaderT :: m (Either a (r -> a)) }

instance Functor f => Functor (MaybeReaderT r f) where fmap f (MaybeReaderT x) = MaybeReaderT $ (f +++ (f.)) <$> x
instance Applicative p => Applicative (MaybeReaderT r p) where
    pure = MaybeReaderT . pure . Left
    MaybeReaderT f <*> MaybeReaderT x = MaybeReaderT (liftA2 go f x)
      where go (Left f) (Left x) = Left (f x)
            go (Left f) (Right x) = Right (f . x)
            go (Right f) (Left x) = Right (($x) . f)
            go (Right f) (Right x) = Right (f <*> x)
instance Alternative p => Alternative (MaybeReaderT r p) where
    empty = MaybeReaderT empty
    MaybeReaderT x <|> MaybeReaderT y = MaybeReaderT (x <|> y)

lift :: Functor f => f a -> MaybeReaderT r f a
lift = MaybeReaderT . fmap Left

splitWhen :: (a -> Bool) -> [a] -> NonEmpty [a]
splitWhen p = go
  where
    go = \ case
        [] -> []:|[]
        a:(splitWhen p -> as:|ass) | p a -> []:|as:ass | True -> (a:as):|ass

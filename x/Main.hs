{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Array
import Data.Bool
import Data.Char
import Data.Foldable
import Data.List (elemIndex, reverse, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Rule.Hex as Hex
import qualified Data.Rule.Moore as Moore
import Options.Applicative
import Text.Read (readMaybe)
import Util
import YFind

import Rule
import qualified Symmetry

main :: IO ()
main = do
    (ruleString, Options {..}) <- execParser (info (unMaybeReaderT options) mempty) >>= \ case
        Left o -> pure o
        Right f -> f . fromMaybe (error "no parse") . readGrid <$> getContents
    let showHeader a = asum ["x = ", show x, ", y = ", show y, ", rule = ", ruleString, "\n"]
          where ((il, jl), (ih, jh)) = bounds a
                (x, y) = (ih - il + 1, jh - jl + 1)
    foldMapA (putStrLn . runReaderT (altMap ReaderT [showHeader, showGrid])) $ case rule' of Rule' rule -> go rule parms

data Options = Options { rule' :: Rule' (Int, Int) Bool, parms :: Parms }

data Rule' i a where Rule' :: (Applicative f, Traversable f) => Rule i f a -> Rule' i a

options :: MaybeReaderT (Array (Int, Int) (Maybe Bool)) Parser ([Char], Options)
options = (\ (ruleString, rule') parms -> (ruleString, Options {..}))
                  <$> lift (option (maybeReader $ liftA2 fmap (,) parseRule') (short 'r' <> metavar "rule"))
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
    parseRule' :: [Char] -> Maybe (Rule' (Int, Int) Bool)
    parseRule' (fmap toUpper -> s)
      | Just s <- stripSuffix "H" s = Rule' . toRule <$> readMaybe @Hex.Rule s
      | otherwise = Rule' . toRule <$> readMaybe @Moore.Rule s <|>
                    Rule' . toRule <$> readMaybe @Hex.Rule   s

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
    splitOn "\n" & (traverse . traverse $ \ case
                        '.' -> Just (Just False)
                        'o' -> Just (Just True)
                        ' ' -> Just Nothing
                        _   -> Nothing) &
    fmap (\ rows ->
          let (width, height) = (maximum $ length <$> rows, length rows)
          in array ((0, 0), (width-1, height-1))
             [((i, j), c) | (j, row) <- zip [0..] rows
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

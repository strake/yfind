module Control.Monad.Trans.MaybeReader where

import Control.Applicative
import Control.Arrow

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

module Util.Monad.Primitive.Unsafe where

import Control.Monad.Base.Control
import Control.Monad.Primitive
import Data.Basic

unsafeInterleaveWhileJust :: (MonadBaseControl m, PrimBase (Base m)) => m (Maybe a) -> (a -> m ()) -> m [a]
unsafeInterleaveWhileJust mma f = go
  where
    go = mma >>= liftBaseOp_ unsafeInterleave . \ case
        Nothing -> pure []
        Just a -> (a :) <$> liftBaseOp_ unsafeInterleave (a `seq` f a *> go)

unsafeInterleaveWhileJust' :: (MonadBaseControl m, PrimBase (Base m)) => ([b] -> m (Maybe (a, b))) -> m [a]
unsafeInterleaveWhileJust' f = go []
  where
    go bs = f bs >>= liftBaseOp_ unsafeInterleave . \ case
        Nothing -> pure []
        Just (a, b) -> (a :) <$> liftBaseOp_ unsafeInterleave (go (b:bs))

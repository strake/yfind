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

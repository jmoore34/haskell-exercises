{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Typeclasses.MonadReader () where
import Monads.Reader(Reader)
import qualified Monads.Reader as Reader
import MonadTransformers.ReaderT(ReaderT)
import qualified MonadTransformers.ReaderT as ReaderT
import Typeclasses.MonadTrans
import MonadTransformers.MaybeT


-- typeclass w/ two type variables that it relates together
-- namely, m is uniquely determined/dependent on e, because
-- Reader/ReaderT is partially applied with e in order to
-- become a monad
class Monad m => MonadReader e m | m -> e where
    -- `m r` instead of `m a` because the "result" is just r
    -- i.e. ask is basically a wrapped \r -> r
    ask :: m e
    ask = reader id   -- i.e. asks (\r -> r)

    -- `reader` is just another name for asks
    reader :: (e -> a) -> m a
    reader f = fmap f ask

    -- like fmapping over the r instead of the a
    -- note that we can't change the type, because the type
    -- r is baked into the monad when we partially apply (Reader r) / (ReaderT r)
    local :: (e -> e) -> m a -> m a

instance MonadReader e (Reader e) where
    ask :: Reader e e
    ask = Reader.ask

    local :: (e -> e) -> Reader e a -> Reader e a
    local = Reader.local

instance (Monad m) => MonadReader e (ReaderT e m) where
    ask :: ReaderT e m e
    ask = ReaderT.ask

    local :: (e -> e) -> ReaderT e m a -> ReaderT e m a
    local = ReaderT.local

-- for all other transformers:
-- instance (MonadTrans t, Monad m, Monad (t m), MonadReader e m) => MonadReader e (t m) where
-- but this would cause overlapping instances

-- e.g. MaybeT
instance (MonadReader e m) => MonadReader e (MaybeT m) where
    ask :: MaybeT m e
    ask = lift ask

    -- local is like fmap, but over the environment e rather than the result value a
    local :: (e -> e) -> MaybeT m a -> MaybeT m a
    local f = MaybeT . (local f) . runMaybeT

-- similar implementation for other monad transformers
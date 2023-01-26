module MonadTransformers.MaybeT () where

-- MaybeT :: (* -> *) -> * -> *
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT m) = MaybeT $ fmap (fmap f) m

instance Monad m => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure = MaybeT . pure . Just
    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    MaybeT mmf <*> MaybeT mma = MaybeT $
        mmf >>= \mf ->
            mma >>= \ma ->
                return $ mf <*> ma

instance Monad m => Monad (MaybeT m) where
    return :: a -> MaybeT m a
    return = MaybeT . return . Just
    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    MaybeT mma >>= f = MaybeT $
        mma >>= \ma -> case ma of
            Nothing -> return Nothing
            Just a -> runMaybeT $ f a
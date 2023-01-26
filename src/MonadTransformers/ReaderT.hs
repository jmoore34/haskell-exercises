module MonadTransformers.ReaderT () where

-- ReaderT :: * -> (* -> *) -> * -> *
newtype ReaderT e m a = ReaderT { runReaderT :: e -> m a }

instance Monad m => Functor (ReaderT e m) where
    fmap :: (a -> b) -> ReaderT e m a -> ReaderT e m b
    fmap f reader = ReaderT $ \e ->
        let ma = runReaderT reader $ e
        in  ma >>= \a ->
            return $ f a

instance Monad m => Applicative (ReaderT e m) where
    pure :: a -> ReaderT e m a
    pure a = ReaderT $ \_ -> return a
    (<*>) :: ReaderT e m (a -> b) -> ReaderT e m a -> ReaderT e m b
    readerF <*> readerA = ReaderT $ \e ->
        let mf = runReaderT readerF $ e
        in  mf >>= \f ->
            let ma = runReaderT readerA $ e
            in  ma >>= \a ->
                return $ f a

instance Monad m => Monad (ReaderT e m) where
    (>>=) :: ReaderT e m a -> (a -> ReaderT e m b) -> ReaderT e m b
    reader >>= f = ReaderT $ \e ->
        let ma = runReaderT reader $ e
        in  ma >>= \a ->
            runReaderT (f a) $ e

ask' :: Monad m => ReaderT e m e
ask' = ReaderT $ \e -> return e

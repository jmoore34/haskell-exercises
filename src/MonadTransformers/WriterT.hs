module MonadTransformers.WriterT () where


-- WriterT :: * -> (* -> *) -> * -> *
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Monad m, Monoid w) => Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f (WriterT m) = WriterT $
        m >>= \(a,w) ->
            return (f a, w)

instance (Monad m, Monoid w) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure a = WriterT $ return (a, mempty)
    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    WriterT mf <*> WriterT ma = WriterT $
        mf >>= \(f, w0) ->
            ma >>= \(a, w1) ->
                return (f a, w0 <> w1)

instance (Monad m, Monoid w) => Monad (WriterT w m) where
    (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    WriterT maw >>= f = WriterT $
        maw >>= \(a, w0) ->
            let mbw = runWriterT $ f a
            in  mbw >>= \(b, w1) ->
                return (b, w0 <> w1)

tell' :: Monad m => w -> WriterT w m ()
tell' w = WriterT $ return ((), w)

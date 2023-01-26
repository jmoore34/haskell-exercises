module MonadTransformers.IdentityT () where

-- IdentityT :: (* -> *) -> * -> *
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance Monad m => Functor (IdentityT m) where
    fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
    fmap f (IdentityT m) = IdentityT $
        fmap f m

instance Monad m => Applicative (IdentityT m) where
    pure :: a -> (IdentityT m a)
    pure a = IdentityT $ return a
    (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
    IdentityT mf <*> IdentityT ma = IdentityT $
        mf <*> ma

instance Monad m => Monad (IdentityT m) where
    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    IdentityT ma >>= f = IdentityT $
        ma >>= \a ->
            runIdentityT $ f a

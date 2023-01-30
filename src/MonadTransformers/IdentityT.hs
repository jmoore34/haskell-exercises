module MonadTransformers.IdentityT () where
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))

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

-- MonadTrans :: (* -> *) -> * -> -> * -> Constraint
-- IdentityT  :: (* -> *) -> * -> -> *
instance MonadTrans IdentityT where
    lift :: (Monad m) => m a -> IdentityT m a
    lift = IdentityT

-- MonadIO ::               (* -> *) -> Constraint
-- IdentityT  :: (* -> *) -> * -> *
instance (MonadIO m) => MonadIO (IdentityT m) where
    liftIO :: IO a -> IdentityT m a
    liftIO = lift . liftIO
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module MonadTransformers.ExceptT () where
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))

newtype ExceptT e m a = ExceptT { unExceptT :: m (Either e a) }

instance (Monad m) => Functor (ExceptT e m) where
    fmap :: (a -> b) -> ExceptT e m a -> ExceptT e m b
    fmap f (ExceptT me) = ExceptT $
        me >>= \e ->
            case e of
                Left l -> pure $ Left l
                Right r -> pure $ Right (f r)

instance (Monad m) => Applicative (ExceptT e m) where
    pure :: a -> ExceptT e m a
    pure = ExceptT . pure . Right

    (<*>) :: ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
    ExceptT mef <*> ExceptT mea = ExceptT $
        mef >>= \ef ->
            mea >>= \ea ->
                pure $ ef <*> ea

instance (Monad m) => Monad (ExceptT e m) where
    (>>=) :: ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
    ExceptT mea >>= f = ExceptT $
        mea >>= \ea ->
            case ea of
                Left l -> pure $ Left l
                Right a -> unExceptT $ f a

instance MonadTrans (ExceptT e) where
    lift :: (Monad m) => m a -> ExceptT e m a
    lift = ExceptT . fmap Right

instance (MonadIO m) => MonadIO (ExceptT e m) where
    liftIO :: IO a -> ExceptT e m a
    -- Turn the `IO a` into a `m a`, then we can just call lift (m a -> ExceptT e m a)
    -- The call to liftIO just does the exact same thing in nested monad transformers,
    -- before finally hitting IO, which just calls `id`. In effect, this recursion is
    -- equivalent to nested calls to `lift`, e.g. `lift . lift . lift ... . lift . id`,
    -- which is what we would do anyway if we wanted to manually lift the IO all the way up
    liftIO = lift . liftIO
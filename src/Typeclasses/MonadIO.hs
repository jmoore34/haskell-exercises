{-# LANGUAGE UndecidableInstances #-}
module Typeclasses.MonadIO () where
import Control.Monad.Trans (MonadTrans(..))

-- MonadIO :: (* -> *) -> Constraint
class Monad m => MonadIO m where
    -- liftIO :: * -> *
    liftIO :: IO a -> m a

-- MonadIO :: (* -> *) -> Constraint
-- IO      ::  * -> *
instance MonadIO IO where
    liftIO :: IO a -> IO a
    liftIO = id

instance (MonadIO m, MonadTrans t, Monad (t m)) => MonadIO (t m) where
    liftIO :: IO a -> t m a
    -- expands into lift . lift . lift ...... . lift . id
    liftIO = lift . liftIO
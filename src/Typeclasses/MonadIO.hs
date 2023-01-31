module Typeclasses.MonadIO () where

-- MonadIO :: (* -> *) -> Constraint
class Monad m => MonadIO m where
    -- liftIO :: * -> *
    liftIO :: IO a -> m a

-- MonadIO :: (* -> *) -> Constraint
-- IO      ::  * -> *
instance MonadIO IO where
    liftIO :: IO a -> IO a
    liftIO = id

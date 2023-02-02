{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE UndecidableInstances #-}
module MonadTransformers.MaybeT (MaybeT(..), lift, runMaybeT) where
import Control.Monad.State (MonadState (..))

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

-- MonadTrans :: ((* -> *) -> * -> *) -> Constraint
class (forall m. Monad m => Monad (t m)) => MonadTrans t where
    lift :: (Monad m) => m a -> t m a

-- MonadTrans :: ((* -> *) -> * -> *) -> Constraint
-- MaybeT     ::  (* -> *) -> * -> *
instance MonadTrans MaybeT where
    lift :: (Monad m) => m a -> MaybeT m a
    -- version 1
    -- lift ma = MaybeT $
    --     fmap Just ma

    -- version 2
    lift = MaybeT . fmap pure

-- MonadIO :: (* -> *) -> Constraint
class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

-- MonadIO    ::            (* -> *) -> Constraint
-- MaybeT     :: (* -> *) -> * -> *
-- (MaybeT m) ::             * -> *      (Monad m)
instance MonadIO m => MonadIO (MaybeT m) where
    liftIO :: IO a -> MaybeT m a
    -- Version 1
    -- liftIO ioA = MaybeT $
    --     let mA :: m a
    --         mA = liftIO ioA
    --     in fmap Just mA

    -- Version 2
    liftIO ioA =
        --  mA :: m a
        let mA = liftIO ioA
        in lift mA

    -- Version 3
    -- liftIO = lift . liftIO

-- Example
readLine :: (Read a) => IO (Maybe a)
readLine = do
    line <- getLine
    pure $ read line
    -- or fmap read getLine

readLine2 :: (Read a) => MaybeT IO a
readLine2 = do
    line <- liftIO getLine
    pure $ read line

-- undecidable instances due to recursive instances
instance (MonadState s m, MonadTrans t) => MonadState s (t m) where
    -- get :: t m s
    -- not to be confused with lift . get
    -- lift :: m a -> t m a
    -- fmapping Just over a State/StateT will map over the a part of `-> (a,s)`
    get = lift get

    -- put :: s -> MaybeT m ()
    put = lift . put
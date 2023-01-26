{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant $" #-}
module Lib
    (
    ) where
import Data.Function (($))

class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$>) :: (a -> b) -> f a -> f b
    (<$>) = fmap

class Functor m => Applicative m where
    pure :: a -> m a
    (<*>) :: m (a -> b) -> m a -> m b
    -- liftA is just fmap defined within Applicative, like
    -- how liftM is just fmap defined within Monad
    liftA :: (a -> b) -> m a -> m b
    liftA f a = pure f <*> a     -- i.e. f <$> a
    liftA2 :: (a -> b -> c) -> m a -> m b -> m c
    liftA2 f a b = pure f <*> a <*> b
    liftA3 :: (a -> b -> c -> d) -> m a -> m b -> m c -> m d
    liftA3 f a b c = pure f <*> a <*> b <*> c

class Applicative m => Monad m where
    return :: a -> m a
    return = pure
    (>>=) :: m a -> (a -> m b) -> m b
    -- liftM is just fmap defined inside Monad,
    -- like how liftA is just fmap defined inside Applicative
    liftM :: (a -> b) -> m a -> m b
    liftM f ma = ma >>= (return . f)
    -- `ap` is just >>= (apply) defined inside Monad
    ap :: m (a -> b) -> m a -> m b
    mf `ap` ma =
        mf >>= (\f ->
            ma >>= (return . f)
        )


data Maybe x = Nothing | Just x

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    fs <*> xs = case fs of
        Nothing -> Nothing
        Just f -> fmap f xs


instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    xs >>= f = case xs of
        Nothing -> Nothing
        Just x -> f x

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f state = State $ \s0 ->
        let (a, s1) = runState state $ s0
            b = f a
        in (b, s1)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a,s)
    (<*>) :: State s (a -> b) -> State s a -> State s b
    sf <*> sa = State $ \s0 ->
        let (f, s1) = runState sf $ s0
            (a, s2) = runState sa $ s1
            b = f a
        in (b, s2)

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    state >>= f = State $ \s0 ->
        let (a, s1) = runState state $ s0
        in runState (f a) s1

get :: State s s
get = State $ \s -> (s,s)

set :: s -> State s ()
set new_state = State $ \_ -> ((),new_state)

newtype Reader e a = Reader { runReader:: e -> a }

instance Functor (Reader e) where
    fmap :: (a -> b) -> Reader e a -> Reader e b
    fmap f reader = Reader $ \e ->
        let a = runReader reader $ e
        in  f a

instance Applicative (Reader e) where
    pure :: a -> Reader e a
    pure a = Reader $ \_ -> a
    (<*>) :: Reader e (a -> b) -> Reader e a -> Reader e b
    rf <*> reader = Reader $ \e ->
        let f = runReader rf $ e
            a = runReader reader $ e
        in f a

instance Monad (Reader e) where
    (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
    reader >>= f = Reader $ \e ->
        let a = runReader reader $ e
        in  runReader (f a) $ e

ask :: Reader e e
ask = Reader $ \e -> e

class Semigroup a where
    (<>) :: a -> a -> a
    sappend :: a -> a -> a
    sappend = (<>)

class Semigroup m => Monoid m where
    mappend :: m -> m -> m
    mappend = (<>)
    mempty :: m
    mconcat :: [m] -> m
    mconcat [] = mempty
    mconcat [m] = m
    mconcat (m:ms) = m <> (mconcat ms)


newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f (Writer (a, w)) =
        Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure a = Writer (a, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    Writer (f, w0) <*> Writer (a, w1) =
        Writer (f a, w0 <> w1)

instance Monoid w => Monad (Writer w) where
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    Writer (a, w0) >>= f =
        let Writer (b, w1) = f a
        in Writer (b, w0 <> w1)

tell :: w -> Writer w w
tell w = Writer (w, w)


newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure :: a -> Identity a
    pure a = Identity a
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (Identity a) >>= f = f a

-- compose
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) a = f (g a)

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a

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

-- StateT :: * -> (* -> *) -> * -> *
newtype StateT s m a = StateT { runStateT :: s -> m (a, s)}

instance Monad m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f state = StateT $ \s0 ->
        let m0 = runStateT state $ s0
        in  m0 >>= \(a,s) ->
            return (f a, s)

instance Monad m => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure a = StateT $ \s -> return (a,s)
    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    sf <*> sa = StateT $ \s0 ->
        let mf = runStateT sf $ s0
        in  mf >>= \(f,s1) ->
            let ma = runStateT sa $ s1
            in  ma >>= \(a,s2) ->
                return (f a, s2)

instance Monad m => Monad (StateT s m) where
    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    state >>= f = StateT $ \s0 ->
        let m = runStateT state $ s0
        in  m >>= \(a, s1) ->
            runStateT (f a) $ s1

get' :: Monad m => StateT s m s
get' = StateT $ \s -> return (s,s)

put' :: Monad m => s -> StateT s m ()
put' s = StateT $ \_ -> return ((),s)

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

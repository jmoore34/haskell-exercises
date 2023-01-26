{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Typeclasses.Monad () where
import Prelude ((.))

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
    liftA f a = f <$> a     -- i.e. pure f <*> a
    liftA2 :: (a -> b -> c) -> m a -> m b -> m c
    liftA2 f a b = f <$> a <*> b
    liftA3 :: (a -> b -> c -> d) -> m a -> m b -> m c -> m d
    liftA3 f a b c = f <$> a <*> b <*> c

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
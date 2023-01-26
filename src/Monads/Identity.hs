module Monads.Identity () where

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
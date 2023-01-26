module Monads.Writer () where

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
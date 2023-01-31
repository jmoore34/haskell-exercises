{-# LANGUAGE LambdaCase #-}
module Monads.Either () where
import Prelude hiding (Right, Left, Either)

-- Either :: * -> * -> *
data Either a b = Left a | Right b

instance Functor (Either l) where
    fmap :: (a -> b) -> Either l a -> Either l b
    fmap f e = case e of
        Left l  -> Left l
        Right r -> Right (f r)

instance Applicative (Either l) where
    pure :: a -> Either l a
    pure = Right

    (<*>) :: Either l (a -> b) -> Either l a -> Either l b
    Left l <*> _ = Left l
    _ <*> Left l = Left l
    Right f <*> Right a = Right (f a)

instance Monad (Either l) where
    (>>=) :: Either l a -> (a -> Either l b) -> Either l b
    Left l >>= _ = Left l
    Right a >>= f = f a
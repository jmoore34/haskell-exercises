{-# LANGUAGE NoImplicitPrelude #-}
module Monads.Maybe () where
    
import Control.Monad ( Monad((>>=)), Functor(fmap) )
import Control.Applicative ( Applicative(pure, (<*>)) )

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
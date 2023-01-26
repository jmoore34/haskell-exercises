{-# LANGUAGE NoImplicitPrelude #-}
module Typeclasses.Monoid () where

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
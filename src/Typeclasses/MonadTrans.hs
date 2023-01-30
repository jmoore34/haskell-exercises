{-# LANGUAGE QuantifiedConstraints #-}
module Typeclasses.MonadTrans () where

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
    lift :: Monad m => m a -> t m a
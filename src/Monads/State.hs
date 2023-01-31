{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TupleSections #-}
module Monads.State () where
import Control.Monad.State.Class(get, put, MonadState (state))
import Control.Monad.State (MonadState)

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

put :: s -> State s ()
put new_state = State $ \_ -> ((),new_state)

-- MonadState takes two type arguments, s and m
-- MonadState :: * -> (* -> *) -> Constraint
instance MonadState s (State s) where
    get :: State s s
    get = Monads.State.get

    put :: s -> State s ()
    put = Monads.State.put

    state :: (s -> (a, s)) -> State s a
    state = State
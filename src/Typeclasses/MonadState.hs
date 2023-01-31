{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use >>" #-}
module Typeclasses.MonadState () where
import Control.Monad.Trans.Class (MonadTrans(lift))

-- MonadState :: * -> (* -> *) -> Constraint
-- This is a multiple-parameter type class (https://wiki.haskell.org/Multi-parameter_type_class)
-- i.e., it's not MonadState (s m), but rather a constraint on both s and m,
-- i.e. it expresses a relationship between different types
-- The | m -> s is a functional dependency. E.g., in
--    class Mult a b c | a b -> c,
-- c is not some freefloating/arbitrary type (so no Mult Int Float String), but rather
-- c is the *unique*, only allowed type that can be implemented (returned) for a & b
-- Here, s depends on m, because the State and StateT take an `s` as a parameter (and hence
-- the type of s is baked into the monad (State s) or the monad (StateT s (...))
class (Monad m) => MonadState s m | m -> s where
    get :: m s
    get = state (\s -> (s,s))

    put :: s -> m ()
    put s = state (\_ -> ((), s))

    state :: (s -> (a, s)) -> m a
    -- state f = do
    --     s0 <- get
    --     let (a1, s1) = f s0
    --     put s1
    --     pure a1

    -- desugared version
    state f =
        get >>= \s0 ->
            let (a1, s1) = f s0
            in put s1 >>= \_ ->
                pure a1


instance (MonadState s m, MonadTrans t, Monad (t m)) => MonadState s (t m) where
    -- get :: t m s
    -- not to be confused with lift . get
    -- lift :: m a -> t m a
    get :: t m s
    get = lift get

    put :: s -> t m ()
    put = lift . put

    state :: (s -> (a, s)) -> t m a
    state = lift . state
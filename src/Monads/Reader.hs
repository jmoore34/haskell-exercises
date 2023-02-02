module Monads.Reader (Reader(..), ask, local) where

newtype Reader e a = Reader { runReader :: e -> a }

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

-- like fmapping over the r instead of the a
-- note that we can't change the type, because the type
-- r is baked into the monad when we partially apply (Reader r)
local :: (e -> e) -> Reader e a -> Reader e a
local f reader = Reader $ \e ->
    runReader reader (f e)
module MonadTransformers.StateT () where

-- StateT :: * -> (* -> *) -> * -> *
newtype StateT s m a = StateT { runStateT :: s -> m (a, s)}

instance Monad m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f state = StateT $ \s0 ->
        let m0 = runStateT state $ s0
        in  m0 >>= \(a,s) ->
            return (f a, s)

instance Monad m => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure a = StateT $ \s -> return (a,s)
    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    sf <*> sa = StateT $ \s0 ->
        let mf = runStateT sf $ s0
        in  mf >>= \(f,s1) ->
            let ma = runStateT sa $ s1
            in  ma >>= \(a,s2) ->
                return (f a, s2)

instance Monad m => Monad (StateT s m) where
    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    state >>= f = StateT $ \s0 ->
        let m = runStateT state $ s0
        in  m >>= \(a, s1) ->
            runStateT (f a) $ s1

get' :: Monad m => StateT s m s
get' = StateT $ \s -> return (s,s)

put' :: Monad m => s -> StateT s m ()
put' s = StateT $ \_ -> return ((),s)
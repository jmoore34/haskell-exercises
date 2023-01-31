# Monad Transformers

## class MonadTrans

```haskell
class (Monad m, Monad (t m)) => MonadTrans t where
    lift :: m a -> t m a
```

example: MaybeT
```haskell
lift :: (Monad m) => m a -> MaybeT m a
lift = MaybeT . fmap Just
```

Monad transformers are monad constructors
- In e.g. `MaybeT (ReaderT Int IO) a`, the `(ReaderT Int IO)` can be thought of a parameter in a monad constructor
- Monad is implemented on the *partially applied* `MaybeT m`, meaning we can bind and get directly to a!
- e.g. `myA <- liftIO $ getLine`
- I.e.  `MaybeT (ReaderT Int IO) a` is `<monad, left to right = inner to outer> a`
    - So reading the monad left to right, it’s `IO (Reader Int (Maybe a))`
    - `MaybeT (ReaderT Int IO) :: * -> *`  (a monad!)
    - Whereas if we just did `IO (Reader Int (Maybe String))`, that’s `:: *` (i.e., concrete type w/o a type variable; not a monad!)
- Advantages of monad transformers over nested monads
    - mtl library: less manual lifting, go straight to liftIO / state / etc
    - You’re constructing a new monad you can use with different types, e.g. `MyStack Int` in one function, `MyStack String` in other function

## MonadIO

```haskell
-- MonadIO :: (* -> *) -> Constraint
class Monad m => MonadIO m where
    -- liftIO :: * -> *
    liftIO :: IO a -> m a

-- MonadIO :: (* -> *) -> Constraint
-- IO      ::  * -> *
instance MonadIO IO where
    liftIO :: IO a -> IO a
    liftIO = id
```

For monad transformers, the implementation is just:

```haskell
liftIO = lift . liftIO
```

* The call to `liftIO` turns the `IO a` into an `m a`, then we can just call `lift` (e.g. `m a -> ExceptT e m a`)
* The call to `liftIO` just does the exact same thing in nested monad transformers (since `m` also implements `MonadIO`)
* ...before finally hitting IO, which just calls `id`.
* In effect, this recursion is
equivalent to nested calls to `lift`, e.g. `lift . lift . lift ... . lift . id`,
-- which is what we would do anyway if we wanted to manually lift the IO all the way up
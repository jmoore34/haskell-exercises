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
- I.e., `MaybeT (...) a` is just a monad over `a`, with the `(...)` describing what wraps the Maybe
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

## MonadState

MonadState is just like MonadIO, just instead of liftIO which lifts an `IO a` into an `m a`, we instead have `state` (basically liftState) which lifts a `s -> (a, s)` into an `m a`.

You can then define `get :: m s` and `put :: s -> m ()` in terms of `state`, or the other way around.

The implementation for State and StateT is just:
```haskell
get = get
put = put
state = State/StateT
```

and the implementation for the other transformers is just:
```haskell
get = lift get       -- get :: m s, lift :: m s -> t m s
put = lift . put     -- put :: s -> m s, so func. composition needed to apply the s
state = lift . state -- state :: (s -> (a,s)) -> m a; func. composition applies the function parameter leaving m a
```

And just like MonadIO, this expands into lift . lift . lift . lift ... get/put/state. Loosely, this will mean a bunch of fmapping over a state function (s -> (a,s)), which will loosely end up with something somewhat like e.g. (s -> (Maybe (Either Err a)), s), but because we're in the outermost transformer (in this case ExceptT), we can bind and get a directly.

## MonadReader

* `reader` is another name for `asks` (`reader :: (r -> a) -> m a`)
* `local` is like fmap over the `r` ([example](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#g:5))
    * `local :: (r -> r) -> m a -> m a`

A minimal definition of MonadReader consists of:
1. `ask` or `reader` (which is just `asks`)
2. `local`

[Helpful slides](https://upload.wikimedia.org/wikiversity/en/d/da/Monad.12.A.MonadReader.20180821.pdf)
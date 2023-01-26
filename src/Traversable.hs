{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Traversable  () where
import Prelude hiding (mapM, traverse, sequence, sequenceA, Traversable)

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative m => (a -> m b) -> t a -> m (t b)
    traverse f xs = sequenceA $ fmap f xs
    sequenceA :: Applicative m => t (m a) -> m (t a)
    sequenceA = traverse id
    sequence :: Monad m => t (m a) -> m (t a)
    sequence = sequenceA
    mapM :: (Monad m) => (a -> m b) -> t a -> m (t b)
    mapM = traverse
    forM :: (Monad m) => t a -> (a -> m b) -> m (t b)
    forM = flip mapM
    traverse_ :: Applicative m => (a -> m b) -> t a -> m ()
    traverse_ f xs = void $ traverse f xs

void :: (Functor m) => m a -> m ()
void = fmap (const ())

-- void :: (Applicative m) => m a -> m ()
-- void _ = pure ()

instance Traversable [] where
    sequenceA :: (Applicative m) => [m a] -> m [a]
    -- sequenceA [] = pure []
    -- sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
    sequenceA xs = foldr f (pure []) xs
        where f :: (Applicative m) => m a -> m [a] -> m [a]
              f ma rest = (:) <$> ma <*> rest

    traverse :: (Applicative m) => (a -> m b) -> [a] -> m [b]
    traverse _ [] = pure []
    traverse f (x:xs) = (:) <$> (f x) <*> traverse f xs
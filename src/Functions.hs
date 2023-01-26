module Functions () where

-- compose
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) a = f (g a)

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a
module Typeclasses.Foldable () where

import Prelude hiding (foldr, foldl, Foldable)

class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

instance Foldable [] where
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ acc []  = acc
    foldr f acc [x] = f x acc
    foldr f acc (x:xs) = f x (foldr f acc xs)

    foldl :: (b -> a -> b) -> b -> [a] -> b
    foldl _ acc [] = acc
    foldl f acc [x] = f acc x
    foldl f acc (x:xs) = foldl f (f acc x) xs

module MyUtil where

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a1, a2) = (f a1, f a2)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a1, c1) = (f a1, c1)

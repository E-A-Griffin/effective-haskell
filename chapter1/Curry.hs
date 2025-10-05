module Curry where

import Prelude

uncurriedAddition :: (Int,Int) -> Int
uncurriedAddition nums =
  let
    a = fst nums
    b = snd nums
  in a + b

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \t -> f (fst t) (snd t)

uncurriedAddition' :: (Int,Int) -> Int
uncurriedAddition' = uncurry' (+)

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \a b -> f ((a,b))

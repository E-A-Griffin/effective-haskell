module Zip where

import Prelude hiding (zipWith)

minLength :: [a] -> [b] -> Int
minLength as bs = pred $ min (length as) (length bs)

zipWith :: (a -> b -> (a,b)) -> [a] -> [b] -> [(a,b)]
zipWith f as bs = [ f (as!!i) (bs!!i) | i <- [0..minLength as bs] ]

zipWith' :: (a -> b -> (a,b)) -> [a] -> [b] -> [(a,b)]
zipWith' f as bs =
  foldl (\acc cur -> acc <> [f (as!!cur) (bs!!cur)]) [] [0..minLength as bs]

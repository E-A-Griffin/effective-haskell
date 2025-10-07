module ReverseFold where

reverseL :: [a] -> [a]
reverseL = foldl (flip (:)) []

reverseR :: [a] -> [a]
reverseR = foldr (\cur acc -> acc <> [cur]) []

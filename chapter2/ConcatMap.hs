module ConcatMap where

import Prelude

concatMapL :: (a -> [b]) -> [a] -> [b]
concatMapL f l = foldl (\acc cur -> acc <> f cur) [] l

concatMapR :: (a -> [b]) -> [a] -> [b]
concatMapR f l = foldr (\cur acc -> (f cur) <> acc) [] l

-- It can be hard to keep the differences between foldl and foldr in your head when
-- you are writing code, so it helps to remember these points:
-- 1. The l in foldl stands for left associative.
-- 2. In a left fold, the initial value is applied first, at the left-hand side of the
-- unrolled expression.
-- 3. In a left fold the accumulator value is the first (left) argument of the
-- function you pass in.
-- 4. The r in foldr stands for right associative.
-- 5. In a right fold, the initial value is applied last, at the right-hand side of
-- an unrolled expression.
-- 6. In a right fold, the accumulator is the second (right) argument of the
-- function that you pass in.

module FoldExamples where
import Prelude hiding (foldl, foldr)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl func carryValue lst =
  if null lst
  then carryValue
  else foldl func (func carryValue (head lst)) (tail lst)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr func carryValue lst =
  if null lst
  then carryValue
  else func (head lst) $ foldr func carryValue (tail lst)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (applyElem f) []
  where
    applyElem f elem accumulator = (f elem) : accumulator

map'' :: (a -> b) -> [a] -> [b]
map'' f xs =
  if null xs then []
  else f (head xs) : map'' f (tail xs)

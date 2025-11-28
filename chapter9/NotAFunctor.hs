{-# LANGUAGE DerivingStrategies #-}

module NotAFunctor (SortedList, insertSorted) where

data SortedList a = Empty | Cons a (SortedList a)
  deriving stock (Eq, Show)

insertSorted :: Ord a => a -> SortedList a -> SortedList a
insertSorted a Empty = Cons a Empty
insertSorted a (Cons b bs)
  | a >= b = Cons b (insertSorted a bs)
  | otherwise = Cons a (Cons b bs)

-- This is impossible
instance Functor SortedList where
  fmap :: (Ord a, Ord b) => (a -> b) -> SortedList a -> SortedList b
  fmap _ Empty = Empty
  fmap f (Cons a as) = insertSorted (f a) (fmap f as)

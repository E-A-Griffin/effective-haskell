{-# LANGUAGE LambdaCase #-}

module List where

data List a = Empty | Cons a (List a)

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons c l) = c : fromList l

toList :: [a] -> List a
toList [] = Empty
toList (f:l) = Cons f $ toList l

toList' :: [a] -> List a
toList' = foldr Cons Empty

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr f acc l =
  case l of
    Empty -> acc
    Cons x xs -> f x (listFoldr f acc xs)

fromList' :: List a -> [a]
fromList' = listFoldr (:) []

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ acc Empty = acc
listFoldl f acc (Cons x xs) =
  listFoldl f (f acc x) xs

fromList'' :: List a -> [a]
fromList'' = listFoldl (\acc c -> acc <> [c]) []

listHead :: List a -> Maybe a
listHead Empty = Nothing
listHead (Cons h _) = Just h

listTail :: List a -> List a
listTail Empty = Empty
listTail (Cons _ t) = t

listReverse :: List a -> List a
listReverse = listFoldl (\acc c -> Cons c acc) Empty

listMap :: (a -> b) -> List a -> List b
listMap f = \case
  Empty -> Empty
  Cons x xs -> Cons (f x) $ listMap f xs

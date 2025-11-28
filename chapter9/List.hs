{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module List where

import Data.String

data List a = Empty | List a (List a)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Empty = Empty
  fmap f (List a as) = List (f a) $ fmap f as

concatLists :: List a -> List a -> List a
concatLists Empty bs = bs
concatLists (List a as) bs = List a $ concatLists as bs

instance Applicative List where
  pure :: a -> List a
  pure a = List a Empty

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) Empty _ = Empty
  (<*>) _ Empty = Empty
  (<*>) (List f fs) as = concatLists (fmap f as) (fs <*> as)

instance Monad List where
  return :: a -> List a
  return = pure

  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) Empty f = Empty
  (>>=) (List a as) f = f a `concatLists` (as >>= f)

fromList :: List a -> [a]
fromList Empty = []
fromList (List a as) = a : fromList as

toList :: [a] -> List a
toList [] = Empty
toList (a:as) = List a $ toList as

type StringL = List Char

instance Show a => Show (List a) where
  show = show . fromList

instance IsString (List Char) where
  fromString = toList


replicateL :: Int -> a -> List a
replicateL 1 x = List x Empty
replicateL n x = List x $ replicateL (n - 1) x

isWhiteSpace :: Char -> Bool
isWhiteSpace c
  | c == ' '  = True
  | c == '\t' = True
  | c == '\r' = True
  | c == '\n' = True
  | otherwise = False

dropWhileL :: (a -> Bool) -> List a -> List a
dropWhileL _ Empty = Empty
dropWhileL pred l@(List a as) = if pred a
  then dropWhileL pred as
  else l

takeWhileL :: (a -> Bool) -> List a -> List a
takeWhileL _ Empty = Empty
takeWhileL pred (List a as) = if pred a
  then List a $ takeWhileL pred as
  else Empty

wordsL :: StringL -> List StringL
wordsL Empty = Empty
wordsL (List a as) = if isWhiteSpace a
  then wordsL (dropWhileL isWhiteSpace as)
  else List (List a (takeWhileL (not . isWhiteSpace) as)) $
       wordsL $ dropWhileL (not . isWhiteSpace) as

unwordsL :: List StringL -> StringL
unwordsL Empty = Empty
unwordsL (List w ws) = concatLists (concatLists w " ") $ unwordsL ws

removeDuplicateSpacesL :: StringL -> StringL
removeDuplicateSpacesL = unwordsL . wordsL

concatL :: List (List a) -> List a
concatL = (>>= id)

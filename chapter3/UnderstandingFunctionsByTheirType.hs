module UnderstandingFunctionsByTheirType where

swap' :: (a,b) -> (b,a)
swap' (a,b) = (b,a)

concat' :: [[a]] -> [a]
concat' l = foldr (<>) mempty l

id' :: a -> a
id' a = a

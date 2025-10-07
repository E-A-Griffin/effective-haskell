module InfiniteFind where

findFirst :: (a -> Bool) -> [a] -> [a]
findFirst predicate =
  foldr findHelper []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound


main :: IO ()
main = print $ findFirst (> 2) (cycle [1..5])

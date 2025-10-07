module Lists where

pairs :: [Int] -> [Int] -> [(Int,Int)]
pairs as bs =
  let as' = filter (`elem` bs) as
      bs' = filter odd bs
      mkPairs a = map (\b -> (a,b)) bs'
  in concat $ map mkPairs as'

pairs' :: [Int] -> [Int] -> [(Int,Int)]
pairs' as bs =
  [(a,b) | a <- as, b <- bs, a `elem` bs, odd b]

combineLists :: [a] -> [b] -> [(a,b)]
combineLists as bs = map (\i -> (as!!i,bs!!i)) [0..(length as - 1)]

pairwiseSum :: Num n => [n] -> [n] -> [n]
pairwiseSum xs ys =
  let sumElems pairs =
        let a = fst pairs
            b = snd pairs
        in a + b
  in map sumElems $ zip xs ys

pairwiseSum' :: Num n => [n] -> [n] -> [n]
pairwiseSum' xs ys = map (uncurry (+)) $ zip xs ys

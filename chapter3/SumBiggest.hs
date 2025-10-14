module SumBiggest where

import Data.Bifunctor
import Data.List

-- | Takes a list of list of numbers and returns a string representation of a
-- | comma-separated list of numbers, where each number is the sum of all
-- | occurrences of the biggest number, minus the sum of all occurrences of the
-- | smallest number
sumBiggest :: [[Int]] -> String
sumBiggest allNums =
  let
    getBiggests :: [Int] -> [Int]
    getBiggests nums = filter (== maximum nums) nums

    getSmallests :: [Int] -> [Int]
    getSmallests nums = filter (== minimum nums) nums

    differences :: ([Int],[Int]) -> Int
    differences = uncurry (flip subtract) . bimap sum sum

    allBiggests :: [[Int]]
    allBiggests = map getBiggests allNums

    allSmallests :: [[Int]]
    allSmallests = map getSmallests allNums

    sizePairs :: [([Int], [Int])]
    sizePairs = zip allBiggests allSmallests

    differences' :: [String]
    differences' = map (show . differences) sizePairs
  in Data.List.intercalate "," differences'

main :: IO ()
main = do
  putStrLn "Should be [\"5\", \"4\", \"3\", \"2\", \"1\"]"
  mapM_ print $ sumBiggest [[1,6],[0,0,1,1,1,1,1,2,2],[4,1,2],[2,0],[2,2,11,2,2,2]]

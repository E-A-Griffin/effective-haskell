module Main where

reduce :: (a -> b -> a) -> a -> [b] -> a
reduce func carryValue lst =
  if null lst then carryValue
  else
    let intermediateValue = func carryValue (head lst)
    in reduce func intermediateValue (tail lst)

isBalanced :: String -> Bool
isBalanced s =
  0 == isBalanced' 0 s
  where
    isBalanced' count s
      | null s = count
      | head s == '(' = isBalanced' (count + 1) (tail s)
      | head s == ')' = isBalanced' (count - 1) (tail s)
      | otherwise = isBalanced' count (tail s)

isBalancedReduce :: String -> Bool
isBalancedReduce s = 0 == reduce isBalanced' 0 s
  where
    isBalanced' count c
      | c == '(' = count + 1
      | c == ')' = count - 1
      | otherwise = count

partialFunction :: Int -> String
partialFunction 0 = "I only work for 0"
partialFunction impossibleValue = error $
  "I only work for 0 but I was called with " <> show impossibleValue

main :: IO ()
main = putStrLn $ partialFunction 3

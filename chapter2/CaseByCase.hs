module CaseByCase where

handleNums :: [Int] -> String
handleNums l =
  case l of
    [] -> "An empty list"
    [x] | x == 0 -> "a list called: [0]"
        | x == 1 -> "a list called: [1]"
        | even x -> "a singleton list containing an even number"
        | otherwise -> "the list contains " <> (show x)
    _list -> "this list has more than 1 element"

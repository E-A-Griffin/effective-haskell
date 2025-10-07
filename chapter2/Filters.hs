module Filters where

checkGuestList :: [String] -> String -> Bool
checkGuestList guestList name =
  name `elem` guestList

foodCosts :: [(String, Double)]
foodCosts =
  [("Ren", 10.00)
  ,("George", 4.00)
  ,("Porter", 27.50)]

partyBudget :: (String -> Bool) -> [(String, Double)] -> Double
partyBudget isAttending =
  foldr (+) 0 . map snd . filter (isAttending . fst)

partyBudget' :: Double
partyBudget' =
  foldr (+) 0
  . map snd
  . filter (\name -> fst name `elem` ["Ren", "Porter"])
  $ [("Ren", 10.00), ("George", 4.00), ("Porter", 27.50)]

partyBudget'' :: (String -> Bool) ->
                 (String -> String -> Bool) ->
                 (String -> Double) ->
                 [(String, String)] ->
                 Double
partyBudget'' isAttending willEat foodCost guests =
  foldl (+) 0 $
  [ foodCost food
  | guest <- map fst guests
  , food <- map snd guests
  , willEat guest food
  , isAttending guest
  ]

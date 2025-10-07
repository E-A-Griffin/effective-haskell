module PatternMatch where

matchTuple :: (String, String) -> String
matchTuple ("hello", "world") = "Hello there, you great big world"
matchTuple ("hello", name) = "Oh, hi there, " <> name
matchTuple (salutation, "George") = "Oh! " <> salutation <> " George!"
matchTuple n = show n

modifyPair :: (String, String) -> String
modifyPair p@(a,b)
  | a == "Hello" = "this is a salutation"
  | b == "George" = "this is a message for George"
  | otherwise = "I don't know what " <> show p <> " means"

module TypeHoleDemo where

exampleNumbers :: [Int]
exampleNumbers = [1..10]

getFiveNumbers :: [Int]
getFiveNumbers = take 5 mempty

permuteThruple :: (a,b,c)
               -> ((a,b,c), (a,c,b), (b,a,c), (b,c,a), (c,a,b), (c,b,a))
permuteThruple (a,b,c) = ((a,b,c), (a,c,b), (b,a,c), (b,c,a), (c,a,b), (c,b,a))

mergeFirstTwo :: (a,b,c) -> (a -> b -> d) -> (d, c)
mergeFirstTwo (a,b,c) f = (f a b, c)

showFields :: String
showFields =
  let (a,b) = combinePermutations . permuteThruple $ ("hello", "world", 10)
  in unlines [fst a, fst b]
  where
    joinFields a b = show a <> " - " <> b
    combinePermutations (a,b,c,d,e,f) =
      ( mergeFirstTwo a joinFields
      , mergeFirstTwo c joinFields
      )

showStringPair :: (String, String) -> String
showStringPair (a,b) = "fst: " <> a <> ", snd: " <> b

doubleField :: a -> (a,a)
doubleField a = (a,a)

showValues :: String
showValues = unlines $ map (showStringPair . doubleField . show) [1..10]

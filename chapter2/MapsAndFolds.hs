module MapsAndFolds where

f0 :: Fractional i => (a -> b) -> (b -> i -> i) -> [a] -> i
f0 f g = foldr g 0 . map f

f1 :: Fractional i => (a -> b) -> (b -> i -> i) -> [a] -> i
f1 f g = foldr (g . f) 0

f2 :: Fractional i => (a -> b) -> (i -> b -> i) -> [a] -> i
f2 f g = foldl g 0 . map f

f3 :: Fractional i => (i -> i) -> (i -> a -> i) -> [a] -> i
f3 f g = foldl (g . f) 0

main :: IO ()
main = do
  putStrLn "f0 (+1) (+) [1..20] = 230 = f1 (+1) (+)"
  putStrLn "f3 (+2) (/) [1..3] = 1.3333333333333333 /= 0.0 = f2 (+2) (/) [1..3]"

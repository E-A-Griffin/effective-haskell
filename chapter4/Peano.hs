module Peano where

data Peano = Z | S Peano

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S . toPeano $ n - 1

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S n) = succ (fromPeano (n :: Peano))

eqPeano :: Peano -> Peano -> Bool
eqPeano p p' =
  case (p,p') of
    (Z,Z) -> True
    (S n, S n') -> eqPeano n n'
    _ -> False

addPeano :: Peano -> Peano -> Peano
addPeano p p' =
  case (p,p') of
    (Z,Z) -> Z
    (Z, S n') -> S n'
    (S n, Z) -> S n
    (S n, S n') -> S . S $ addPeano n $ n'

-- wow!
addPeano' :: Peano -> Peano -> Peano
addPeano' Z b = b
addPeano' (S a) b = addPeano a (S b)

main :: IO ()
main = do
  let s = show $ fromPeano (S (S (Z)))
      s' = show . fromPeano $ toPeano 2
  putStrLn $
    "fromPeano (S (S (Z))) = fromPeano $ toPeano 2 = 2: " <>
    s                                                     <>
    " = "                                                 <>
    s'                                                    <>
    " = "                                                 <>
    "2"

  pure ()

module FizzBuzz where

fizzBuzzFor number
  | 0 == number `rem` 15 = "fizzbuzz"
  | 0 == number `rem` 5 = "buzz"
  | 0 == number `rem` 3 = "fizz"
  | otherwise = show number

naiveFizzBuzz fizzBuzzCount curNum fizzBuzzString =
  if curNum > fizzBuzzCount
  then fizzBuzzString
  else
    let nextFizzBuzzString = fizzBuzzString <> fizzBuzzFor curNum <> " "
        nextNumber = curNum + 1
    in naiveFizzBuzz fizzBuzzCount nextNumber nextFizzBuzzString

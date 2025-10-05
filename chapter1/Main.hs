module Main where

guardSize num
  | num > 0 =
      let size = "positive"
      in exclaim size
  | num < 3 = exclaim "small"
  | num < 100 = exclaim "medium"
  | otherwise = exclaim "large"
  where
    exclaim message = "that's a " <> message <> " number!"

makeGreeting salutation person =
  let messageWithTrailingSpace = salutation <> " "
  in messageWithTrailingSpace <> person

extendedGreeting salutation person =
  let hello = makeGreeting "Hello" person
      goodDay = makeGreeting "I hope you have a nice afternoon" person
      goodBye = makeGreeting "See you later" person
  in hello <> "\n" <> goodDay <> "\n" <> goodBye

main = print $ makeGreeting "Hello" "Greeting"

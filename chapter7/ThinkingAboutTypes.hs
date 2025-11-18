{-# LANGUAGE ExplicitForAll #-}
module ThinkingAboutTypes where

import Prelude
import Data.Functor ( (<&>) )

wrapIO :: a -> IO (IO a)
wrapIO = pure . pure

unwrapIO :: IO (IO a) -> IO a
unwrapIO = (>>= id)

readN :: forall a. Read a => Int -> [IO a]
readN n
  | n == 0    = []
  | otherwise = do
      ln :: IO a <- pure readLn
      ln : readN (n-1)

moveIO :: [IO a] -> IO [a]
moveIO [] = pure []
moveIO (h:t) = do
  h' :: a <- h
  (h' :) <$> moveIO t

{-# LANGUAGE LambdaCase #-}
module CommandLineCalculator where

import Prelude
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Text.Read
import Control.Monad
import Data.Foldable (Foldable(foldl'))

data Op = Plus | Minus | Mult

readOp :: IO (Maybe Op)
readOp = getLine <&> \case { "+" -> Just Plus; "-" -> Just Minus; "*" -> Just Mult; _ -> Nothing }

readNums :: IO [Int]
readNums = getLine >>= (\case { Nothing -> pure []; Just x -> (x :) <$> readNums }) . readMaybe

difference :: [Int] -> Int
difference [] = 0
difference (h:t) = foldl' (flip subtract) h t

main :: IO ()
main = do
  op <- readOp
  case op of
    Nothing -> print "Invalid op! Use \"+\". \"-\", or \"*\""
    Just o -> do
        nums <- readNums
        let f = case o of
              Plus  -> sum
              Minus -> difference
              Mult  -> product
        print $ f nums

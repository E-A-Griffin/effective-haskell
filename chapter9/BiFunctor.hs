{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

module BiFunctor where

import Data.Bifunctor

data Myther a b = LeftM a | RightM b
  deriving stock (Eq)

instance Functor (Myther a) where
  fmap :: (b -> c) -> Myther a b -> Myther a c
  fmap f e = case e of
    (LeftM l) -> LeftM l
    (RightM r) -> RightM (f r)

instance Bifunctor Myther where
  bimap :: (a -> b) -> (c -> d) -> Myther a c -> Myther b d
  bimap lf rf = \case
    (LeftM a) -> LeftM $ lf a
    (RightM b) -> RightM $ rf b


main :: IO ()
main = do
  let e0 = LeftM 0
      e1 = RightM 'a'

  print $ (bimap id id e0 :: Myther Int Int) == e0
  print $ (bimap id id e1 :: Myther Char Char) == e1

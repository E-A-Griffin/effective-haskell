module Contravariant where

import Data.Functor.Contravariant

newtype Function a b = Function
  { runFunction :: a -> b }

instance Functor (Function a) where
  fmap :: (b -> c) -> Function a b -> Function a c
  fmap f (Function g) = Function $ f . g

instance Applicative (Function a) where
  pure :: b -> Function a b
  pure x = Function $ const x

  (<*>) :: forall a b c. Function a (b -> c) -> Function a b -> Function a c
  (<*>) (Function f) (Function g) = Function $ \value -> f value (g value)

-- Doesn't seem possible
instance Contravariant (Function a) where
  contramap :: forall a b c. (c -> b) -> Function a b -> Function a c
  contramap f (Function g) = Function $ f . g

class Profunctor f where
  dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

  lmap :: (c -> a) -> f a b -> f c b
  lmap f = dimap f id

  rmap :: (b -> d) -> f a b -> f a d
  rmap f = dimap id f

instance Profunctor Function where
  dimap :: forall a b c d. (c -> a) -> (b -> d) -> Function a b -> Function c d
  dimap f g (Function h) = Function $ g . h . f

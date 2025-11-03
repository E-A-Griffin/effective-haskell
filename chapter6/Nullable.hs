{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}


module Nullable where
import Prelude hiding (null)
import Data.Kind (Type)

class  Nullable a where
  isNull :: a -> Bool
  -- TODO: Ask about LambdaCase variant with only one case
  -- isNull = \case
  --   null -> True
  --   _    -> False
  -- generates a warning about redundant case
  null   :: a

newtype BasicNullable a = BasicNullable (Maybe a)
  deriving stock Show

instance Nullable (BasicNullable a) where
  isNull (BasicNullable Nothing) = True
  isNull _ = False
  null = BasicNullable Nothing

newtype TransitiveNullable a = TransitiveNullable (Maybe a)
  deriving stock Show

instance Nullable a => Nullable (TransitiveNullable a) where
  isNull (TransitiveNullable Nothing) = True
  isNull (TransitiveNullable (Just x)) = isNull x
  null = TransitiveNullable Nothing

instance Nullable a => Nullable (Maybe a) where
  isNull = \case
    Nothing -> True
    _       -> False

  null = Nothing

instance (Nullable a, Nullable b) => Nullable (a,b) where
  isNull (a,b)
    | isNull a && isNull b = True
    | otherwise            = False

  null = (null, null)

instance Eq a => Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []

newtype OptionalString = OptionalString { getString :: Maybe String }
  deriving stock Show
  deriving Nullable via BasicNullable String

newtype OptionalNonEmptyString = OptionalNonEmptyString { getNonEmptyString :: Maybe String }
  deriving stock Show
  deriving Nullable via TransitiveNullable String

module Oden.Metadata where

data Metadata a = Metadata a

instance Show (Metadata a) where
  show _ = "<meta>"

instance Eq (Metadata a) where
  _ == _ = True

instance Ord a => Ord (Metadata a) where
  _ `compare` _ = EQ

unwrap :: Metadata a -> a
unwrap (Metadata v) = v

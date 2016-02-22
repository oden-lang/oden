module Oden.Type.Kind where

data Kind = Type | TypeConstructor Kind Kind deriving (Eq)

instance Show Kind where
  show Type = "*"
  show (TypeConstructor Type k2) =
    "* -> " ++ show k2
  show (TypeConstructor (TypeConstructor k1 k2) k3) =
    "(" ++ show k1 ++ " -> " ++ show k2 ++ ") -> " ++ show k3

module Oden.Identifier where

type Name = String

data Identifier = Unqualified Name
                | Qualified Name Name
                deriving (Eq, Ord)

instance Show Identifier where
  show (Unqualified name) = name
  show (Qualified package name) = package ++ "." ++ name

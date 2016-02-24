module Oden.QualifiedName where

type PackageName = String
type Name = String

data QualifiedName = FQN [PackageName] Name
                   deriving (Show, Eq, Ord)

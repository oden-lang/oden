module Oden.QualifiedName where

import Oden.Identifier

type PackageName = String

data QualifiedName = FQN [PackageName] Identifier
                   deriving (Show, Eq, Ord)

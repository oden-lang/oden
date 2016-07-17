module Oden.QualifiedName where

import Oden.Identifier

data PackageName
  = NativePackageName [String]
  | ForeignPackageName String
  deriving (Show, Eq, Ord)

data QualifiedName
  = FQN PackageName Identifier
  deriving (Show, Eq, Ord)

nameInUniverse :: String -> QualifiedName
nameInUniverse = FQN (NativePackageName []) . Identifier

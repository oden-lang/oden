module Oden.SourceFile where

import Oden.QualifiedName

data SourceFile = OdenSourceFile FilePath PackageName
                deriving (Show, Eq, Ord)

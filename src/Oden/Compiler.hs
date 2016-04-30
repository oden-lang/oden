module Oden.Compiler where

import           Oden.Compiler.Monomorphization
import           Oden.Core.Resolved

import           Control.Arrow

data CompilationError = MonomorphError MonomorphError
                      deriving (Show, Eq, Ord)

compile :: ResolvedPackage -> Either CompilationError MonomorphedPackage
compile = left MonomorphError . monomorphPackage

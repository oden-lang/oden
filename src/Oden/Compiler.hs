module Oden.Compiler where

import           Oden.Core.Monomorphed
import           Oden.Core.Resolved

import           Oden.Compiler.Monomorphization

import           Control.Arrow

data CompilationError = MonomorphError MonomorphError
                      deriving (Show, Eq, Ord)

compile :: ResolvedPackage -> Either CompilationError MonomorphedPackage
compile = left MonomorphError . monomorphPackage

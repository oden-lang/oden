module Oden.Compiler where

import           Oden.Compiler.Monomorphization
import           Oden.Core

import           Control.Arrow

data CompilationError = MonomorphError MonomorphError
                      deriving (Show, Eq, Ord)

compile :: TypedPackage -> Either CompilationError MonomorphedPackage
compile = left MonomorphError . monomorphPackage

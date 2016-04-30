module Oden.Compiler where

import           Oden.Core.Monomorphed
import           Oden.Core.Typed

import           Oden.Compiler.Monomorphization

import           Control.Arrow

data CompilationError = MonomorphError MonomorphError
                      deriving (Show, Eq, Ord)

compile :: TypedPackage -> Either CompilationError MonomorphedPackage
compile = left MonomorphError . monomorphPackage

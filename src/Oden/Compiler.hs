module Oden.Compiler where

import           Oden.Predefined
import           Oden.Environment

import           Oden.Core.Monomorphed
import           Oden.Core.Typed

import           Oden.Compiler.Environment
import           Oden.Compiler.Monomorphization

import           Control.Arrow

data CompilationError = MonomorphError MonomorphError
                      deriving (Show, Eq, Ord)


environmentFromPackage :: TypedPackage -> CompileEnvironment
environmentFromPackage pkg@(TypedPackage _ imports _) =
  fromPackage universe `merge` fromPackage pkg `merge` fromPackages imports

compile :: CompileEnvironment
        -> TypedPackage
        -> Either CompilationError MonomorphedPackage
compile env pkg = left MonomorphError $ monomorphPackage env pkg

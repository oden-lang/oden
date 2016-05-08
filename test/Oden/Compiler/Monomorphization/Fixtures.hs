module Oden.Compiler.Monomorphization.Fixtures where

import           Oden.Core.Package

import           Oden.QualifiedName
import           Oden.Metadata
import           Oden.SourceInfo

import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

missing :: Metadata SourceInfo
missing = Metadata Missing

myPkg :: PackageDeclaration
myPkg = PackageDeclaration missing ["my", "pkg"]

tvA :: Poly.TVar
tvA = Poly.TV "a"

a :: Poly.Type
a = Poly.TVar missing tvA

typeInt = Poly.TCon missing (nameInUniverse "int")
typeBool = Poly.TCon missing (nameInUniverse "bool")

boolToBool = Poly.TFn missing typeBool typeBool

monoInt = Mono.TCon missing (nameInUniverse "int")
monoBool = Mono.TCon missing (nameInUniverse "bool")

module Oden.Compiler.Monomorphization.Fixtures where

import           Oden.Core.Package

import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo

import           Oden.Compiler
import           Oden.Compiler.Monomorphization as Monomorphization

import qualified Oden.Type.Monomorphic          as Mono
import qualified Oden.Type.Polymorphic          as Poly

monomorphPackage pkg =
  Monomorphization.monomorphPackage
  (environmentFromPackage pkg)
  pkg

missing :: Metadata SourceInfo
missing = Metadata Missing

myPkg :: PackageDeclaration
myPkg = PackageDeclaration missing (NativePackageName ["my", "pkg"])

dependencyPkg :: PackageDeclaration
dependencyPkg = PackageDeclaration missing (NativePackageName ["dependency", "pkg"])

tvA :: Poly.TVar
tvA = Poly.TV "a"

a :: Poly.Type
a = Poly.TVar missing tvA

typeInt = Poly.TCon missing (nameInUniverse "int")
typeBool = Poly.TCon missing (nameInUniverse "bool")

boolToBool = Poly.TFn missing typeBool typeBool

monoInt = Mono.TCon missing (nameInUniverse "int")
monoBool = Mono.TCon missing (nameInUniverse "bool")

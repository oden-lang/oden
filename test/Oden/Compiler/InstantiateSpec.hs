module Oden.Compiler.InstantiateSpec where

import           Test.Hspec

import           Oden.Compiler.Instantiate
import qualified Oden.Core                 as Core
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic     as Mono
import qualified Oden.Type.Polymorphic     as Poly

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

tvA :: Poly.TVar
tvA = Poly.TV "a"

tvarA :: Poly.Type
tvarA = Poly.TVar missing tvA

typeInt = Poly.TCon missing (nameInUniverse "int")
typeBool = Poly.TCon missing (nameInUniverse "bool")
typeString = Poly.TCon missing (nameInUniverse "string")

monoInt = Mono.TCon missing (nameInUniverse "int")
monoBool = Mono.TCon missing (nameInUniverse "bool")
monoString = Mono.TCon missing (nameInUniverse "string")

identityPoly :: Core.Expr Poly.Type
identityPoly =
  Core.Fn
  missing
  (Core.NameBinding missing (Identifier "x"))
  (Core.Symbol missing (Identifier "x") tvarA)
  (Poly.TFn missing tvarA tvarA)

identityIntType :: Mono.Type
identityIntType = Mono.TFn missing monoInt monoInt

identityInt :: Core.Expr Poly.Type
identityInt =
  Core.Fn
  missing
  (Core.NameBinding missing (Identifier "x"))
  (Core.Symbol missing (Identifier "x") typeInt)
  (Poly.TFn missing typeInt typeInt)

lenType :: Poly.Type
lenType = Poly.TUncurriedFn missing [Poly.TSlice missing tvarA] typeInt

lenPoly :: Core.Expr Poly.Type
lenPoly = Core.Symbol missing (Identifier "len") lenType

lenIntType :: Mono.Type
lenIntType = Mono.TUncurriedFn missing [Mono.TSlice missing monoInt] monoInt

lenInt :: Core.Expr Poly.Type
lenInt =
  Core.Symbol
  missing
  (Identifier "len")
  (Poly.TUncurriedFn missing [Poly.TSlice missing typeInt] typeInt)

pairPoly :: Core.Expr Poly.Type
pairPoly =
  Core.Tuple
  missing
  (Core.Symbol missing (Identifier "x") tvarA)
  (Core.Symbol missing (Identifier "y") (Poly.TVar missing (Poly.TV "b")))
  []
  (Poly.TTuple missing tvarA (Poly.TVar missing (Poly.TV "b")) [])


pairIntStringType :: Mono.Type
pairIntStringType = Mono.TTuple missing monoInt monoString []

pairIntString :: Core.Expr Poly.Type
pairIntString =
  Core.Tuple
  missing
  (Core.Symbol missing (Identifier "x") typeInt)
  (Core.Symbol missing (Identifier "y") typeString)
  []
  (Poly.TTuple missing typeInt typeString [])

spec :: Spec
spec =
  describe "instantiate" $ do
    it "does not change monomorphic function" $
      instantiate identityInt identityIntType `shouldSucceedWith` identityInt
    it "instantiates identity function with int" $
      instantiate identityPoly identityIntType `shouldSucceedWith` identityInt
    it "instantiates len func" $
      instantiate lenPoly lenIntType `shouldSucceedWith` lenInt
    it "instantiates polymorphic tuple" $
      instantiate pairPoly pairIntStringType `shouldSucceedWith` pairIntString

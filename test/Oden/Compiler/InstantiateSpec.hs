module Oden.Compiler.InstantiateSpec where

import           Test.Hspec

import           Oden.Compiler.Instantiate
import qualified Oden.Core                 as Core
import           Oden.Identifier
import qualified Oden.Type.Monomorphic     as Mono
import qualified Oden.Type.Polymorphic     as Poly

import           Oden.Assertions

identityPoly :: Core.Expr Poly.Type
identityPoly =
     Core.Fn "x" (Core.Symbol (Unqualified "x") (Poly.TVar (Poly.TV "a")))
                 (Poly.TFn (Poly.TVar (Poly.TV "a")) (Poly.TVar (Poly.TV "a")))

identityIntType :: Mono.Type
identityIntType = Mono.TFn Mono.typeInt Mono.typeInt

identityInt :: Core.Expr Poly.Type
identityInt =
     Core.Fn "x" (Core.Symbol (Unqualified "x") Poly.typeInt)
                 (Poly.TFn Poly.typeInt Poly.typeInt)

lenType :: Poly.Type
lenType = Poly.TUncurriedFn [Poly.TSlice (Poly.TVar (Poly.TV "a"))] Poly.typeInt

lenPoly :: Core.Expr Poly.Type
lenPoly = Core.Symbol (Unqualified "len") lenType

lenIntType :: Mono.Type
lenIntType = Mono.TUncurriedFn [Mono.TSlice Mono.typeInt] Mono.typeInt

lenInt :: Core.Expr Poly.Type
lenInt = Core.Symbol
          (Unqualified "len")
          (Poly.TUncurriedFn [Poly.TSlice Poly.typeInt] Poly.typeInt)

spec :: Spec
spec =
  describe "instantiate" $ do
    it "does not change monomorphic function" $
      instantiate identityInt identityIntType `shouldSucceedWith` identityInt
    it "instantiates identity function with int" $
      instantiate identityPoly identityIntType `shouldSucceedWith` identityInt
    it "instantiates len func" $
      instantiate lenPoly lenIntType `shouldSucceedWith` lenInt

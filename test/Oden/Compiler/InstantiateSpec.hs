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
                 (Poly.TArr (Poly.TVar (Poly.TV "a")) (Poly.TVar (Poly.TV "a")))

identityIntType :: Mono.Type
identityIntType = Mono.TArr Mono.typeInt Mono.typeInt

identityInt :: Core.Expr Poly.Type
identityInt =
     Core.Fn "x" (Core.Symbol (Unqualified "x") Poly.typeInt)
                 (Poly.TArr Poly.typeInt Poly.typeInt)

spec :: Spec
spec =
  describe "instantiate" $ do
    it "does not changed monomorphic function" $
      instantiate identityInt identityIntType `shouldSucceedWith` identityInt
    it "instantiates identity function with int" $
      instantiate identityPoly identityIntType `shouldSucceedWith` identityInt

module Oden.CompilerSpec where

import           Data.Set              as Set
import           Test.Hspec

import           Oden.Compiler
import qualified Oden.Core             as Core
import qualified Oden.Env              as Env
import           Oden.Identifier
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

import           Oden.Assertions

myPkg :: Core.PackageName
myPkg = ["my", "pkg"]

tvA :: Poly.TVar
tvA = Poly.TV "a"

a :: Poly.Type
a = Poly.TVar tvA

identityDef :: Core.Definition
identityDef =
  Core.Definition
    "identity"
    (Poly.Forall [tvA] (Poly.TArr a a),
     Core.Fn "x" (Core.Symbol (Unqualified "x") a)
                 (Poly.TArr a a))

identity2Def :: Core.Definition
identity2Def =
  Core.Definition
    "identity2"
    (Poly.Forall [tvA] (Poly.TArr a a),
     Core.Fn "x" (Core.Application (Core.Symbol (Unqualified "identity") (Poly.TArr a a))
                                   (Core.Symbol (Unqualified "x") a)
                                   a)
                 (Poly.TArr a a))

usingIdentityDef :: Core.Definition
usingIdentityDef =
  Core.Definition
    "using-identity"
    (Poly.Forall [] Poly.typeInt,
     Core.Application (Core.Symbol (Unqualified "identity") (Poly.TArr Poly.typeInt Poly.typeInt))
                      (Core.Literal (Core.Int 1) Poly.typeInt)
                      Poly.typeInt)

usingIdentity2Def :: Core.Definition
usingIdentity2Def =
  Core.Definition
    "using-identity2"
    (Poly.Forall [] Poly.typeInt,
     Core.Application (Core.Symbol (Unqualified "identity2") (Poly.TArr Poly.typeInt Poly.typeInt))
                      (Core.Literal (Core.Int 1) Poly.typeInt)
                      Poly.typeInt)

usingIdentityMonomorphed :: MonomorphedDefinition
usingIdentityMonomorphed =
  MonomorphedDefinition
    "using-identity"
    (Core.Application (Core.Symbol (Unqualified "identity_inst_int_to_int") (Mono.TArr Mono.typeInt Mono.typeInt))
                      (Core.Literal (Core.Int 1) Mono.typeInt)
                      Mono.typeInt)

letBoundIdentity :: Core.Definition
letBoundIdentity =
  Core.Definition
    "let-bound-identity"
    (Poly.Forall [] Poly.typeInt,
     Core.Let "identity" (Core.Fn "x" (Core.Symbol (Unqualified "x") a) (Poly.TArr a a))
                         (Core.Application (Core.Symbol (Unqualified "identity") (Poly.TArr Poly.typeInt Poly.typeInt))
                                           (Core.Literal (Core.Int 1) Poly.typeInt)
                                           Poly.typeInt)
                      Poly.typeInt)

usingIdentity2Monomorphed :: MonomorphedDefinition
usingIdentity2Monomorphed =
  MonomorphedDefinition
    "using-identity2"
    (Core.Application (Core.Symbol (Unqualified "identity2_inst_int_to_int") (Mono.TArr Mono.typeInt Mono.typeInt))
                      (Core.Literal (Core.Int 1) Mono.typeInt)
                      Mono.typeInt)

letBoundIdentityMonomorphed :: MonomorphedDefinition
letBoundIdentityMonomorphed =
  MonomorphedDefinition
    "let-bound-identity"
    (Core.Let "identity_inst_int_to_int" (Core.Fn "x" (Core.Symbol (Unqualified "x") Mono.typeInt) (Mono.TArr Mono.typeInt Mono.typeInt))
                                         (Core.Application (Core.Symbol (Unqualified "identity_inst_int_to_int") (Mono.TArr Mono.typeInt Mono.typeInt))
                                                           (Core.Literal (Core.Int 1) Mono.typeInt)
                                                           Mono.typeInt)
                      Mono.typeInt)

identityInstIntToInt :: InstantiatedDefinition
identityInstIntToInt =
  InstantiatedDefinition
    "identity_inst_int_to_int"
    (Core.Fn "x" (Core.Symbol (Unqualified "x") Mono.typeInt)
                 (Mono.TArr Mono.typeInt Mono.typeInt))

identity2InstIntToInt :: InstantiatedDefinition
identity2InstIntToInt =
  InstantiatedDefinition
    "identity2_inst_int_to_int"
    (Core.Fn "x" (Core.Application
                    (Core.Symbol (Unqualified "identity_inst_int_to_int") (Mono.TArr Mono.typeInt Mono.typeInt))
                    (Core.Symbol (Unqualified "x") Mono.typeInt)
                    Mono.typeInt)
                 (Mono.TArr Mono.typeInt Mono.typeInt))

spec :: Spec
spec = do
  describe "compile" $ do
    it "compiles empty package" $
      compile Env.empty (Core.Package myPkg [] [])
      `shouldSucceedWith`
      CompiledPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "excludes unused polymorphic definitions" $
      compile Env.empty (Core.Package myPkg [] [identityDef])
      `shouldSucceedWith`
      CompiledPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "monomorphs polymorphic function usage" $
      compile Env.empty (Core.Package myPkg [] [identityDef, usingIdentityDef])
      `shouldSucceedWith`
      CompiledPackage
        myPkg
        []
        (Set.singleton identityInstIntToInt)
        (Set.singleton usingIdentityMonomorphed)
    it "monomorphs polymorphic function usage recursively" $
      compile Env.empty (Core.Package myPkg [] [identityDef, identity2Def, usingIdentity2Def])
      `shouldSucceedWith`
      CompiledPackage
        myPkg
        []
        (Set.fromList [identityInstIntToInt, identity2InstIntToInt])
        (Set.singleton usingIdentity2Monomorphed)
    it "monomorphs let bound polymorphic function" $
      compile Env.empty (Core.Package myPkg [] [letBoundIdentity])
      `shouldSucceedWith`
      CompiledPackage
        myPkg
        []
        Set.empty
        (Set.singleton letBoundIdentityMonomorphed)

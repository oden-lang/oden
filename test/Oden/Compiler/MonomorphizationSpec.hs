module Oden.Compiler.MonomorphizationSpec where

import           Data.Set              as Set
import           Test.Hspec

import           Oden.Compiler.Monomorphization
import qualified Oden.Core             as Core
import           Oden.Identifier
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly
import           Oden.Type.Basic

import           Oden.Assertions

myPkg :: Core.PackageDeclaration
myPkg = Core.PackageDeclaration Missing ["my", "pkg"]

tvA :: Poly.TVar
tvA = Poly.TV "a"

a :: Poly.Type
a = Poly.TVar Missing tvA

typeInt = Poly.TBasic Missing TInt
typeBool = Poly.TBasic Missing TBool

monoInt = Mono.TBasic Missing TInt
monoBool = Mono.TBasic Missing TBool

identityDef :: Core.Definition
identityDef =
  Core.Definition
    Missing
    (Identifier "identity")
    (Poly.Forall Missing [Poly.TVarBinding Missing tvA] (Poly.TFn Missing a a),
     Core.Fn Missing (Core.NameBinding Missing (Identifier "x")) (Core.Symbol Missing (Identifier "x") a)
                 (Poly.TFn Missing a a))

identity2Def :: Core.Definition
identity2Def =
  Core.Definition
    Missing
    (Identifier "identity2")
    (Poly.Forall Missing [Poly.TVarBinding Missing tvA] (Poly.TFn Missing a a),
     Core.Fn Missing (Core.NameBinding Missing (Identifier "x")) (Core.Application Missing (Core.Symbol Missing (Identifier "identity") (Poly.TFn Missing a a))
                                   (Core.Symbol Missing (Identifier "x") a)
                                   a)
                 (Poly.TFn Missing a a))

usingIdentityDef :: Core.Definition
usingIdentityDef =
  Core.Definition
    Missing
    (Identifier "using-identity")
    (Poly.Forall Missing [] typeInt,
     Core.Application Missing
                      (Core.Symbol Missing (Identifier "identity") (Poly.TFn Missing typeInt typeInt))
                      (Core.Literal Missing (Core.Int 1) typeInt)
                      typeInt)

usingIdentity2Def :: Core.Definition
usingIdentity2Def =
  Core.Definition
    Missing
    (Identifier "using-identity2")
    (Poly.Forall Missing [] typeInt,
     Core.Application Missing
                      (Core.Symbol Missing (Identifier "identity2") (Poly.TFn Missing typeInt typeInt))
                      (Core.Literal Missing (Core.Int 1) typeInt)
                      typeInt)

usingIdentityMonomorphed :: MonomorphedDefinition
usingIdentityMonomorphed =
  MonomorphedDefinition
    Missing
    (Identifier "using-identity")
    monoInt
    (Core.Application Missing
                      (Core.Symbol Missing (Identifier "identity_inst_int_to_int") (Mono.TFn Missing monoInt monoInt))
                      (Core.Literal Missing (Core.Int 1) monoInt)
                      monoInt)

letBoundIdentity :: Core.Definition
letBoundIdentity =
  Core.Definition
    Missing
    (Identifier "let-bound-identity")
    (Poly.Forall Missing [] typeInt,
     Core.Let Missing (Core.NameBinding Missing (Identifier "identity")) (Core.Fn Missing (Core.NameBinding Missing (Identifier "x")) (Core.Symbol Missing (Identifier "x") a) (Poly.TFn Missing a a))
                         (Core.Application
                          Missing
                          (Core.Symbol Missing (Identifier "identity") (Poly.TFn Missing typeInt typeInt))
                          (Core.Literal Missing (Core.Int 1) typeInt)
                          typeInt)
                      typeInt)

usingIdentity2Monomorphed :: MonomorphedDefinition
usingIdentity2Monomorphed =
  MonomorphedDefinition
    Missing
    (Identifier "using-identity2")
    monoInt
    (Core.Application
     Missing
     (Core.Symbol Missing (Identifier "identity2_inst_int_to_int") (Mono.TFn Missing monoInt monoInt))
     (Core.Literal Missing (Core.Int 1) monoInt)
     monoInt)

letBoundIdentityMonomorphed :: MonomorphedDefinition
letBoundIdentityMonomorphed =
  MonomorphedDefinition
    Missing
    (Identifier "let-bound-identity")
    monoInt
    (Core.Let
     Missing
     (Core.NameBinding Missing (Identifier "identity_inst_int_to_int"))
     (Core.Fn Missing (Core.NameBinding Missing (Identifier "x")) (Core.Symbol Missing (Identifier "x") monoInt) (Mono.TFn Missing monoInt monoInt))
     (Core.Application Missing (Core.Symbol Missing (Identifier "identity_inst_int_to_int") (Mono.TFn Missing monoInt monoInt))
      (Core.Literal Missing (Core.Int 1) monoInt)
      monoInt)
     monoInt)

identityInstIntToInt :: InstantiatedDefinition
identityInstIntToInt =
  InstantiatedDefinition
    (Identifier "identity_inst_int_to_int")
    (Core.Fn
     Missing
     (Core.NameBinding Missing (Identifier "x"))
     (Core.Symbol Missing (Identifier "x") monoInt)
     (Mono.TFn Missing monoInt monoInt))

identity2InstIntToInt :: InstantiatedDefinition
identity2InstIntToInt =
  InstantiatedDefinition
    (Identifier "identity2_inst_int_to_int")
    (Core.Fn
     Missing
     (Core.NameBinding Missing (Identifier "x"))
     (Core.Application
      Missing
      (Core.Symbol Missing (Identifier "identity_inst_int_to_int") (Mono.TFn Missing monoInt monoInt))
      (Core.Symbol Missing (Identifier "x") monoInt)
      monoInt)
     (Mono.TFn Missing monoInt monoInt))

sliceLenDef :: Core.Definition
sliceLenDef =
  Core.Definition
    Missing
    (Identifier "slice-len")
    (Poly.Forall Missing [] typeInt,
     Core.UncurriedFnApplication
      Missing
      (Core.Symbol Missing (Identifier "len") (Poly.TUncurriedFn Missing [Poly.TSlice Missing typeBool] typeInt))
      [Core.Slice Missing [Core.Literal Missing (Core.Bool True) typeBool] (Poly.TSlice Missing typeBool)]
      typeInt)

sliceLenMonomorphed :: MonomorphedDefinition
sliceLenMonomorphed =
  MonomorphedDefinition
    Missing
    (Identifier "slice-len")
    monoInt
    (Core.UncurriedFnApplication
     Missing
     (Core.Symbol Missing (Identifier "len") (Mono.TUncurriedFn Missing [Mono.TSlice Missing monoBool] monoInt))
     [Core.Slice Missing [Core.Literal Missing (Core.Bool True) monoBool] (Mono.TSlice Missing (Mono.TBasic Missing TBool))]
     monoInt)

letWithShadowing :: Core.Definition
letWithShadowing =
  Core.Definition
    Missing
    (Identifier "let-with-shadowing")
    (Poly.Forall Missing [] typeInt,
     Core.Let
     Missing
     (Core.NameBinding Missing (Identifier "x"))
     (Core.Literal Missing (Core.Int 1) typeInt)
     (Core.Let
      Missing
      (Core.NameBinding Missing (Identifier "x"))
      (Core.Symbol Missing (Identifier "x") typeInt)
      (Core.Symbol Missing (Identifier "x") typeInt)
      typeInt)
     typeInt)

letWithShadowingMonomorphed :: MonomorphedDefinition
letWithShadowingMonomorphed =
  MonomorphedDefinition
    Missing
    (Identifier "let-with-shadowing")
    monoInt
    (Core.Let
     Missing
     (Core.NameBinding Missing (Identifier "x"))
     (Core.Literal Missing (Core.Int 1) monoInt)
     (Core.Let
      Missing
      (Core.NameBinding Missing (Identifier "x"))
      (Core.Symbol Missing (Identifier "x") monoInt)
      (Core.Symbol Missing (Identifier "x") monoInt)
      monoInt)
     monoInt)

spec :: Spec
spec =
  describe "monomorphPackage" $ do
    it "compiles empty package" $
      monomorphPackage (Core.Package myPkg [] [])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "excludes unused polymorphic definitions" $
      monomorphPackage (Core.Package myPkg [] [identityDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "monomorphs polymorphic function usage" $
      monomorphPackage (Core.Package myPkg [] [identityDef, usingIdentityDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        (Set.singleton identityInstIntToInt)
        (Set.singleton usingIdentityMonomorphed)
    it "monomorphs polymorphic function usage recursively" $
      monomorphPackage (Core.Package myPkg [] [identityDef, identity2Def, usingIdentity2Def])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        (Set.fromList [identityInstIntToInt, identity2InstIntToInt])
        (Set.singleton usingIdentity2Monomorphed)
    it "monomorphs let bound polymorphic function" $
      monomorphPackage (Core.Package myPkg [] [letBoundIdentity])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton letBoundIdentityMonomorphed)
    it "uses polymorphic predefined Go funcs" $
      monomorphPackage (Core.Package myPkg [] [sliceLenDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton sliceLenMonomorphed)
    it "monomorphs let with shadowing" $
      monomorphPackage (Core.Package myPkg [] [letWithShadowing])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton letWithShadowingMonomorphed)

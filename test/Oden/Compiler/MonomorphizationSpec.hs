module Oden.Compiler.MonomorphizationSpec where

import           Data.Set              as Set

import           Test.Hspec

import           Oden.Compiler.Monomorphization
import qualified Oden.Core             as Core
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly

import           Oden.Assertions

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

monoInt = Mono.TCon missing (nameInUniverse "int")
monoBool = Mono.TCon missing (nameInUniverse "bool")

identityDef :: Core.Definition
identityDef =
  Core.Definition
    missing
    (Identifier "identity")
    (Poly.Forall missing [Poly.TVarBinding missing tvA] Set.empty (Poly.TFn missing a a),
     Fn missing (NameBinding missing (Identifier "x")) (Symbol missing (Identifier "x") a)
                 (Poly.TFn missing a a))

identity2Def :: Core.Definition
identity2Def =
  Core.Definition
    missing
    (Identifier "identity2")
    (Poly.Forall missing [Poly.TVarBinding missing tvA] Set.empty (Poly.TFn missing a a),
     Fn missing (NameBinding missing (Identifier "x")) (Application missing (Symbol missing (Identifier "identity") (Poly.TFn missing a a))
                                   (Symbol missing (Identifier "x") a)
                                   a)
                 (Poly.TFn missing a a))

usingIdentityDef :: Core.Definition
usingIdentityDef =
  Core.Definition
    missing
    (Identifier "using-identity")
    (Poly.Forall missing [] Set.empty typeInt,
     Application missing
                      (Symbol missing (Identifier "identity") (Poly.TFn missing typeInt typeInt))
                      (Literal missing (Int 1) typeInt)
                      typeInt)

usingIdentity2Def :: Core.Definition
usingIdentity2Def =
  Core.Definition
    missing
    (Identifier "using-identity2")
    (Poly.Forall missing [] Set.empty typeInt,
     Application missing
                      (Symbol missing (Identifier "identity2") (Poly.TFn missing typeInt typeInt))
                      (Literal missing (Int 1) typeInt)
                      typeInt)

usingIdentityMonomorphed :: MonomorphedDefinition
usingIdentityMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "using-identity")
    monoInt
    (Application missing
                      (Symbol missing (Identifier "identity_inst_int_to_int") (Mono.TFn missing monoInt monoInt))
                      (Literal missing (Int 1) monoInt)
                      monoInt)

letBoundIdentity :: Core.Definition
letBoundIdentity =
  Core.Definition
    missing
    (Identifier "let-bound-identity")
    (Poly.Forall missing [] Set.empty typeInt,
     Let missing (NameBinding missing (Identifier "identity")) (Fn missing (NameBinding missing (Identifier "x")) (Symbol missing (Identifier "x") a) (Poly.TFn missing a a))
                         (Application
                          missing
                          (Symbol missing (Identifier "identity") (Poly.TFn missing typeInt typeInt))
                          (Literal missing (Int 1) typeInt)
                          typeInt)
                      typeInt)

usingIdentity2Monomorphed :: MonomorphedDefinition
usingIdentity2Monomorphed =
  MonomorphedDefinition
    missing
    (Identifier "using-identity2")
    monoInt
    (Application
     missing
     (Symbol missing (Identifier "identity2_inst_int_to_int") (Mono.TFn missing monoInt monoInt))
     (Literal missing (Int 1) monoInt)
     monoInt)

letBoundIdentityMonomorphed :: MonomorphedDefinition
letBoundIdentityMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "let-bound-identity")
    monoInt
    (Let
     missing
     (NameBinding missing (Identifier "identity_inst_int_to_int"))
     (Fn missing (NameBinding missing (Identifier "x")) (Symbol missing (Identifier "x") monoInt) (Mono.TFn missing monoInt monoInt))
     (Application missing (Symbol missing (Identifier "identity_inst_int_to_int") (Mono.TFn missing monoInt monoInt))
      (Literal missing (Int 1) monoInt)
      monoInt)
     monoInt)

identityInstIntToInt :: InstantiatedDefinition
identityInstIntToInt =
  InstantiatedDefinition
    (Identifier "identity")
    missing
    (Identifier "identity_inst_int_to_int")
    (Fn
     missing
     (NameBinding missing (Identifier "x"))
     (Symbol missing (Identifier "x") monoInt)
     (Mono.TFn missing monoInt monoInt))

identity2InstIntToInt :: InstantiatedDefinition
identity2InstIntToInt =
  InstantiatedDefinition
    (Identifier "identity2")
    missing
    (Identifier "identity2_inst_int_to_int")
    (Fn
     missing
     (NameBinding missing (Identifier "x"))
     (Application
      missing
      (Symbol missing (Identifier "identity_inst_int_to_int") (Mono.TFn missing monoInt monoInt))
      (Symbol missing (Identifier "x") monoInt)
      monoInt)
     (Mono.TFn missing monoInt monoInt))

sliceLenDef :: Core.Definition
sliceLenDef =
  Core.Definition
    missing
    (Identifier "slice-len")
    (Poly.Forall missing [] Set.empty typeInt,
     Application
      missing
      (Symbol missing (Identifier "len") (Poly.TForeignFn missing False [Poly.TSlice missing typeBool] [typeInt]))
      (Slice missing [Literal missing (Bool True) typeBool] (Poly.TSlice missing typeBool))
      typeInt)

sliceLenMonomorphed :: MonomorphedDefinition
sliceLenMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "slice-len")
    monoInt
    (Application
     missing
     (Symbol missing (Identifier "len") (Mono.TForeignFn missing False [Mono.TSlice missing monoBool] [monoInt]))
     (Slice missing [Literal missing (Bool True) monoBool] (Mono.TSlice missing monoBool))
     monoInt)

letWithShadowing :: Core.Definition
letWithShadowing =
  Core.Definition
    missing
    (Identifier "let-with-shadowing")
    (Poly.Forall missing [] Set.empty typeInt,
     Let
     missing
     (NameBinding missing (Identifier "x"))
     (Literal missing (Int 1) typeInt)
     (Let
      missing
      (NameBinding missing (Identifier "x"))
      (Symbol missing (Identifier "x") typeInt)
      (Symbol missing (Identifier "x") typeInt)
      typeInt)
     typeInt)

letWithShadowingMonomorphed :: MonomorphedDefinition
letWithShadowingMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "let-with-shadowing")
    monoInt
    (Let
     missing
     (NameBinding missing (Identifier "x"))
     (Literal missing (Int 1) monoInt)
     (Let
      missing
      (NameBinding missing (Identifier "x"))
      (Symbol missing (Identifier "x") monoInt)
      (Symbol missing (Identifier "x") monoInt)
      monoInt)
     monoInt)

spec :: Spec
spec =
  describe "monomorphPackage" $ do
    it "compiles empty package" $
      monomorphPackage (Package myPkg [] [])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "excludes unused polymorphic definitions" $
      monomorphPackage (Package myPkg [] [identityDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        Set.empty
    it "monomorphs polymorphic function usage" $
      monomorphPackage (Package myPkg [] [identityDef, usingIdentityDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        (Set.singleton identityInstIntToInt)
        (Set.singleton usingIdentityMonomorphed)
    it "monomorphs polymorphic function usage recursively" $
      monomorphPackage (Package myPkg [] [identityDef, identity2Def, usingIdentity2Def])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        (Set.fromList [identityInstIntToInt, identity2InstIntToInt])
        (Set.singleton usingIdentity2Monomorphed)
    it "monomorphs let bound polymorphic function" $
      monomorphPackage (Package myPkg [] [letBoundIdentity])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton letBoundIdentityMonomorphed)
    it "uses polymorphic predefined Go funcs" $
      monomorphPackage (Package myPkg [] [sliceLenDef])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton sliceLenMonomorphed)
    it "monomorphs let with shadowing" $
      monomorphPackage (Package myPkg [] [letWithShadowing])
      `shouldSucceedWith`
      MonomorphedPackage
        myPkg
        []
        Set.empty
        (Set.singleton letWithShadowingMonomorphed)

module Oden.Compiler.Monomorphization.ResolvedReferencesSpec where

import           Data.Set              as Set

import           Test.Hspec

import           Oden.Compiler.Monomorphization

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Monomorphed
import           Oden.Core.Typed
import           Oden.Core.ProtocolImplementation

import           Oden.Identifier
import           Oden.QualifiedName
import qualified Oden.Type.Polymorphic as Poly

import           Oden.Compiler.Monomorphization.Fixtures
import           Oden.Assertions

fooProtocolMethod =
  Poly.ProtocolMethod
  missing
  (Identifier "foo")
  (Poly.Forall missing [] Set.empty a)

fooProtocol =
  Poly.Protocol
  missing
  (FQN [] (Identifier "Foo"))
  a
  [fooProtocolMethod]

fooMethodImplementation :: MethodImplementation TypedExpr
fooMethodImplementation =
  MethodImplementation
  missing
  fooProtocolMethod
  (Literal missing (Int 1) typeInt)

fooImplementation :: ProtocolImplementation TypedExpr
fooImplementation =
  ProtocolImplementation
  missing
  fooProtocol
  typeInt
  [fooMethodImplementation]

intFooMethodInstance :: InstantiatedDefinition
intFooMethodInstance =
  InstantiatedMethod
  missing
  (Identifier "Foo_method_foo_inst_int")
  (Literal missing (Int 1) monoInt)

resolvedReference :: TypedDefinition
resolvedReference =
  Definition
    missing
    (Identifier "resolvedReference")
    (Poly.Forall missing [] Set.empty typeInt,
     MethodReference
     missing
     (Resolved
      fooProtocol
      fooProtocolMethod
      fooMethodImplementation)
     typeInt)

resolvedReferenceMonomorphed :: MonomorphedDefinition
resolvedReferenceMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "resolvedReference")
    monoInt
    (Symbol missing (Identifier "Foo_method_foo_inst_int") monoInt)

spec :: Spec
spec = -- do
  it "monomorphs method implementation as a definition" $
    monomorphPackage (TypedPackage myPkg [] [resolvedReference])
    `shouldSucceedWith`
    MonomorphedPackage
      myPkg
      []
      (Set.singleton intFooMethodInstance)
      (Set.singleton resolvedReferenceMonomorphed)

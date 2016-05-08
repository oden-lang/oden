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

fooProtocolName = FQN [] (Identifier "Foo")
fooMethodName = Identifier "foo"

fooProtocol =
  Poly.Protocol
  missing
  fooProtocolName
  a
  [fooProtocolMethod]

fooProtocolMethod =
  Poly.ProtocolMethod
  missing
  fooMethodName
  (Poly.Forall missing [] Set.empty a)

fooImplementation :: ProtocolImplementation TypedExpr
fooImplementation =
  ProtocolImplementation
  missing
  fooProtocolName
  typeInt
  [fooMethodImplementation]

fooMethodImplementation :: MethodImplementation TypedExpr
fooMethodImplementation =
  MethodImplementation
  missing
  fooMethodName
  (Literal missing (Int 1) typeInt)

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
      fooProtocolName
      fooMethodName
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

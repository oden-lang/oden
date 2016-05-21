module Oden.Compiler.Monomorphization.ResolvedReferencesSpec where

import           Data.Set              as Set

import           Test.Hspec

import           Oden.Compiler.Monomorphization

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Monomorphed
import           Oden.Core.Operator
import           Oden.Core.Typed
import           Oden.Core.ProtocolImplementation

import           Oden.Identifier
import           Oden.QualifiedName
import qualified Oden.Type.Polymorphic as Poly
import qualified Oden.Type.Monomorphic as Mono

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

resolvedReferenceFooExpr :: TypedDefinition
resolvedReferenceFooExpr =
  Definition
    missing
    (Identifier "resolvedReferenceFooExpr")
    (Poly.Forall missing [] Set.empty typeInt,
     MethodReference
     missing
     (Resolved
      fooProtocolName
      fooMethodName
      fooMethodImplementation)
     typeInt)

resolvedReferenceFooMonomorphed :: MonomorphedDefinition
resolvedReferenceFooMonomorphed =
  MonomorphedDefinition
    missing
    (Identifier "resolvedReferenceFooExpr")
    monoInt
    (Symbol missing (Identifier "Foo_method_foo_inst_int") monoInt)

resolvedReferenceAddExpr :: TypedDefinition
resolvedReferenceAddExpr =
  Definition
    missing
    (Identifier "resolvedReferenceAddExpr")
    (Poly.Forall missing [] Set.empty (Poly.TFn missing typeInt typeInt),
     MethodReference
     missing
     (Resolved
      (nameInUniverse "Addition")
      (Identifier "Add")
      (MethodImplementation
       missing
       (Identifier "Add")
       (Foreign missing (ForeignOperator Add) (Poly.TFn missing typeInt (Poly.TFn missing typeInt typeInt)))))
      typeInt)

resolvedReferenceAddMonomorphed :: MonomorphedDefinition
resolvedReferenceAddMonomorphed =
  MonomorphedDefinition
  missing
  (Identifier "resolvedReferenceAddExpr")
  (Mono.TFn missing monoInt monoInt)
  (Foreign missing (ForeignOperator Add) (Mono.TFn missing monoInt (Mono.TFn missing monoInt monoInt)))

spec :: Spec
spec = do
  it "monomorphs method implementation as a definition" $
    monomorphPackage (TypedPackage myPkg [] [resolvedReferenceFooExpr])
    `shouldSucceedWith`
    MonomorphedPackage
      myPkg
      []
      (Set.singleton intFooMethodInstance)
      (Set.singleton resolvedReferenceFooMonomorphed)

  it "inlines monomorphed method implementation with foreign expr" $
    monomorphPackage (TypedPackage myPkg [] [resolvedReferenceAddExpr])
    `shouldSucceedWith`
    MonomorphedPackage
      myPkg
      []
      Set.empty
      (Set.singleton resolvedReferenceAddMonomorphed)

module Oden.Infer.InferConstraintsSpec where

import           Test.Hspec

import           Oden.Core.Typed
import           Oden.Core.Expr
import           Oden.Core.Untyped
import           Oden.Identifier
import           Oden.Infer            (inferExpr)
import           Oden.Predefined
import           Oden.Pretty           ()
import           Oden.Type.Polymorphic

import qualified Data.Set as Set

import           Oden.Assertions
import           Oden.Infer.Fixtures

spec :: Spec
spec = describe "inferExpr" $ do
  it "infers type with constraints" $
    let constraint = ProtocolConstraint missing testableProtocolName tvarA
        methodType = TConstrained (Set.singleton constraint) (typeFn tvarA typeBool) in
    inferExpr predefAndTestableProtocol (MethodReference missing (NamedMethodReference (Identifier "Testable") (Identifier "test")) Untyped)
    `shouldSucceedWith`
    (Forall predefined [tvarBinding tvA] (Set.singleton constraint) methodType,
     MethodReference
     missing
     (Unresolved testableProtocolName testableMethodName constraint)
     methodType)

  it "infers multiple usages of method" $
    inferExpr
    predefAndTestableProtocol
    (Tuple
     missing
     (Application
      missing
      (MethodReference missing (NamedMethodReference (Identifier "Testable") (Identifier "test")) Untyped)
      (Literal missing (Int 1) Untyped)
      Untyped)
     (Application
      missing
      (MethodReference missing (NamedMethodReference (Identifier "Testable") (Identifier "test")) Untyped)
      (Literal missing (Bool True) Untyped)
      Untyped)
     []
     Untyped)
    `shouldSucceedWith`
    let boolConstraint = ProtocolConstraint missing testableProtocolName typeBool
        intConstraint = ProtocolConstraint missing testableProtocolName typeInt in
    (Forall
     predefined
     []
     Set.empty
     (TTuple missing typeBool typeBool []),
     Tuple
     missing
     (Application
      missing
      (MethodReference
       missing
       (Unresolved testableProtocolName testableMethodName intConstraint)
       (TConstrained
        (Set.singleton intConstraint)
        (TFn missing typeInt typeBool)))
      (Literal missing (Int 1) typeInt)
      typeBool)
     (Application
      missing
      (MethodReference
       missing
       (Unresolved testableProtocolName testableMethodName boolConstraint)
       (TConstrained
        (Set.singleton boolConstraint)
        (TFn missing typeBool typeBool)))
      (Literal missing (Bool True) typeBool)
      typeBool)
     []
     (TTuple missing typeBool typeBool []))

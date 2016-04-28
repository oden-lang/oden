module Oden.Compiler.ResolutionSpec where

import           Oden.Compiler.Resolution
import           Oden.Compiler.Resolution.Environment
import           Oden.Core
import           Oden.Core.Expr
import           Oden.Core.ProtocolImplementation
import           Oden.Environment
import           Oden.Identifier
import           Oden.Predefined
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Data.Set as Set

import           Test.Hspec

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

predefined :: Metadata SourceInfo
predefined = Metadata Predefined

predef :: ResolutionEnvironment
predef = fromPackage universe

tvA = TV "a"

tvarA = TVar predefined tvA

aToBool = TFn predefined tvarA typeBool

unresolved protocol method =
  MethodReference missing (UnresolvedMethodReference protocol method)

resolved protocol method implMethod =
  MethodReference missing (ResolvedMethodReference protocol method implMethod)

testableProtocolMethod =
  ProtocolMethod
  predefined
  (Identifier "test")
  (Forall predefined [] Set.empty (TFn predefined tvarA typeBool))

testableProtocol  =
  Protocol
  predefined
  (FQN [] (Identifier "Testable"))
  (TVar predefined tvA)
  [testableProtocolMethod]

boolTestableImplementationMethod :: MethodImplementation TypedExpr
boolTestableImplementationMethod =
  MethodImplementation
  missing
  testableProtocolMethod
  undefined

boolTestableImplementation :: ProtocolImplementation TypedExpr
boolTestableImplementation =
  ProtocolImplementation
  missing
  testableProtocol
  [boolTestableImplementationMethod]

predefAndTestableProtocol :: ResolutionEnvironment
predefAndTestableProtocol =
  predef
  `extend`
  (Identifier "Testable",
   ImplementationBinding predefined boolTestableImplementation)

spec :: Spec
spec =
  describe "resolveInExpr" $ do

    it "throws error if there's no matching implementation" $
      shouldFail $
        resolveInDefinition
        predef
        (Definition
         missing
         (Identifier "foo")
         (Forall missing [] (Set.singleton (ProtocolConstraint missing testableProtocol tvarA)) aToBool,
          unresolved
          testableProtocol
          testableProtocolMethod
          aToBool))

    it "resolves a single matching implementation" $
      resolveInDefinition
      predefAndTestableProtocol
      (Definition
        missing
        (Identifier "foo")
        (Forall missing [] (Set.singleton (ProtocolConstraint missing testableProtocol tvarA)) aToBool,
        unresolved
        testableProtocol
        testableProtocolMethod
        aToBool))
      `shouldSucceedWith`
      Definition
      missing
      (Identifier "foo")
      (Forall missing [] (Set.singleton (ProtocolConstraint missing testableProtocol tvarA)) aToBool,
       resolved
       testableProtocol
       testableProtocolMethod
       boolTestableImplementationMethod
       aToBool)


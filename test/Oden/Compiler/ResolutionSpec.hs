module Oden.Compiler.ResolutionSpec where

import           Oden.Compiler.Resolution

import           Oden.Core.Expr
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Typed

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

tvA = TV "a"

tvarA = TVar predefined tvA
symbol s = Symbol missing (Identifier s)

boolToBool = TFn predefined typeBool typeBool
aToBool = TFn predefined tvarA typeBool

one = Literal missing (Int 1) typeInt

unresolved protocol method =
  MethodReference missing (Unresolved protocol method)

resolved protocol method implMethod =
  MethodReference missing (Resolved protocol method implMethod)

testableProtocolMethod =
  ProtocolMethod
  predefined
  (Identifier "test")
  (Forall predefined [] Set.empty (TFn predefined tvarA typeBool))

testableProtocol =
  Protocol
  predefined
  (FQN [] (Identifier "Testable"))
  (TVar predefined tvA)
  [testableProtocolMethod]

boolTestableMethod :: String -> MethodImplementation TypedExpr
boolTestableMethod s =
  MethodImplementation missing testableProtocolMethod (symbol s boolToBool)

boolTestableImplementation :: String -> ProtocolImplementation TypedExpr
boolTestableImplementation s =
  ProtocolImplementation missing testableProtocol typeBool [boolTestableMethod s]

rowWithResultField =
  rowFromList [(Identifier "result", typeBool)] (REmpty missing)

recordTestableMethod :: MethodImplementation TypedExpr
recordTestableMethod =
  MethodImplementation
  missing
  testableProtocolMethod
  (symbol "recordValue" (TFn missing rowWithResultField typeBool))

recordTestableImplementation :: ProtocolImplementation TypedExpr
recordTestableImplementation =
  ProtocolImplementation
  missing
  testableProtocol
  rowWithResultField
  [recordTestableMethod]

spec :: Spec
spec =
  describe "resolveInExpr" $ do

    it "throws error if there's no matching implementation" $
      shouldFail $
        resolveInExpr
        Set.empty
        (unresolved
         testableProtocol
         testableProtocolMethod
         aToBool)

    it "resolves a single matching implementation" $
      resolveInExpr
      (Set.singleton (boolTestableImplementation "myImpl"))
      (unresolved
       testableProtocol
       testableProtocolMethod
       boolToBool)
      `shouldSucceedWith`
      resolved
      testableProtocol
      testableProtocolMethod
      (boolTestableMethod "myImpl")
      boolToBool

    it "resolves a single matching implementation for a record type" $
      resolveInExpr
      (Set.singleton recordTestableImplementation)
      (Application
       missing
       (unresolved
        testableProtocol
        testableProtocolMethod
        (TFn missing rowWithResultField typeBool))
       (symbol "recordValue" rowWithResultField)
       typeBool)
      `shouldSucceedWith`
      Application
      missing
      (resolved
       testableProtocol
       testableProtocolMethod
       recordTestableMethod
       (TFn missing rowWithResultField typeBool))
      (symbol "recordValue" rowWithResultField)
      typeBool

    it "throws error if there's multiple matching implementations" $
      resolveInExpr
      (Set.fromList [ boolTestableImplementation "myImpl"
                    , boolTestableImplementation "myOtherImpl"
                    ])
      (unresolved
       testableProtocol
       testableProtocolMethod
       aToBool)
      `shouldFailWith`
      MultipleMatchingImplementationsInScope
      Missing
      [ boolTestableImplementation "myImpl"
      , boolTestableImplementation "myOtherImpl"
      ]

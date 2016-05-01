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

testableProtocolMethodInt =
  ProtocolMethod
  predefined
  (Identifier "test")
  (Forall predefined [] Set.empty (TFn predefined typeInt typeBool))

testableProtocolInt =
  Protocol
  predefined
  (FQN [] (Identifier "Testable"))
  typeInt
  [testableProtocolMethod]

symbol s = Symbol missing (Identifier s) aToBool

boolTestableMethod s =
  MethodImplementation missing testableProtocolMethod (symbol s)

boolTestableImplementation s =
  ProtocolImplementation missing testableProtocol typeBool [boolTestableMethod s]

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
       aToBool)
      `shouldSucceedWith`
      resolved
      testableProtocol
      testableProtocolMethod
      (boolTestableMethod "myImpl")
      aToBool

    it "resolves a single matching implementation for a less general type" $
      resolveInExpr
      (Set.singleton (boolTestableImplementation "myImpl"))
      (Application
       missing
       (unresolved
        testableProtocolInt
        testableProtocolMethodInt
        boolToBool)
       one
       typeBool)
      `shouldSucceedWith`
      Application
      missing
      (resolved
       testableProtocolInt
       testableProtocolMethodInt
       (boolTestableMethod "myImpl")
       boolToBool)
      one
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

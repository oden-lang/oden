module Oden.Compiler.ResolutionSpec where

import           Oden.Compiler.Resolution

import           Oden.Core
import           Oden.Core.Expr
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Resolved

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

tvA = TV "a"

tvarA = TVar predefined tvA

boolToBool = TFn predefined typeBool typeBool
aToBool = TFn predefined tvarA typeBool

one = Literal missing (Int 1) typeInt

unresolved protocol method type' =
  MethodReference missing (UnresolvedMethodReference protocol method) type'

resolved protocol method implMethod type' =
  MethodReference missing (ResolvedMethodReference protocol method implMethod) type'

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

symbol s = Symbol missing (Identifier s) aToBool

boolTestableMethod s =
  MethodImplementation missing testableProtocolMethod (symbol s)

boolTestableImplementation s =
  ProtocolImplementation missing testableProtocol [boolTestableMethod s]

spec :: Spec
spec =
  describe "resolveInExpr" $ do

    it "throws error if there's no matching implementation" $
      shouldFail $
        resolveInExpr
        []
        (unresolved
         testableProtocol
         testableProtocolMethod
         aToBool)

    it "resolves a single matching implementation" $
      resolveInExpr
      [boolTestableImplementation "myImpl"]
      (unresolved
       testableProtocol
       testableProtocolMethod
       aToBool)
      `shouldSucceedWith`
      (resolved
       testableProtocol
       testableProtocolMethod
       (boolTestableMethod "myImpl")
       aToBool)

    it "resolves a single matching implementation for a less general type" $
      resolveInExpr
      [boolTestableImplementation "myImpl"]
      (Application
       missing
       (unresolved
        testableProtocol
        testableProtocolMethod
        boolToBool)
       one
       typeBool)
      `shouldSucceedWith`
      (Application
       missing
       (resolved
        testableProtocol
        testableProtocolMethod
        (boolTestableMethod "myImpl")
        boolToBool)
       one
       typeBool)

    it "throws error if there's multiple matching implementations" $
      resolveInExpr
      [ boolTestableImplementation "myImpl"
      , boolTestableImplementation "myOtherImpl"
      ]
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

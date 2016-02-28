module Oden.Infer.SubsumptionSpec where

import           Test.Hspec

import           Oden.Infer.Subsumption
import           Oden.SourceInfo
import           Oden.Core
import           Oden.Identifier
import           Oden.QualifiedName
import           Oden.Type.Basic
import           Oden.Type.Polymorphic

import           Oden.Assertions


tvarA :: Type
tvarA = TVar Predefined (TV "a")

tvarB :: Type
tvarB = TVar Predefined (TV "b")

tvarC :: Type
tvarC = TVar Predefined (TV "c")

scheme:: Type -> Scheme
scheme= Forall Predefined []

typeUnit, typeAny, typeInt, typeString :: Type
typeUnit = TUnit Predefined
typeAny = TAny Predefined
typeInt = TBasic Predefined TInt
typeString = TBasic Predefined TString

typeFn :: Type -> Type -> Type
typeFn = TFn Missing

named :: String -> Type -> Type
named = TNamed Missing . FQN []

typeSlice :: Type -> Type
typeSlice = TSlice Missing

spec :: Spec
spec =
  describe "subsumedBy" $ do

    it "any is subsumed by any" $
      scheme typeAny `subsumedBy` Literal Predefined Unit typeAny
      `shouldSucceedWith`
      (scheme typeAny, Literal Predefined Unit typeAny)

    it "any is subsumed by int" $
      scheme typeAny `subsumedBy` Literal Predefined Unit typeUnit
      `shouldSucceedWith`
      (scheme typeAny, Literal Predefined Unit typeUnit)

    it "int is not subsumed by any" $
      shouldFail (scheme typeInt `subsumedBy` Literal Predefined Unit typeAny)

    it "tvar is not subsumed by any" $
      shouldFail (scheme tvarA `subsumedBy` Literal Predefined Unit typeAny)

    it "any is subsumed by tvar" $
      scheme typeAny `subsumedBy` Literal Predefined Unit tvarA
      `shouldSucceedWith`
      (scheme typeAny, Literal Predefined Unit typeAny)

    it "tvar is subsumed by same tvar" $
      scheme tvarA `subsumedBy` Literal Predefined Unit tvarA
      `shouldSucceedWith`
      (scheme tvarA, Literal Predefined Unit tvarA)

    it "tvar b is subsumed by tvar a and all a tvars are substituted" $
      scheme tvarB `subsumedBy` Literal Predefined Unit tvarA
      `shouldSucceedWith`
      (scheme tvarB, Literal Predefined Unit tvarB)

    it "int is subsumed by int" $
      scheme typeInt `subsumedBy` Literal Predefined (Int 1) typeInt
      `shouldSucceedWith`
      (scheme typeInt, Literal Predefined (Int 1) typeInt)

    it "string is subsumed by string" $
      scheme typeString `subsumedBy` Literal Predefined (String "foo") typeString
      `shouldSucceedWith`
      (scheme typeString, Literal Predefined (String "foo") typeString)

    it "string is not subsumed by int" $
      shouldFail (scheme typeString `subsumedBy` Literal Predefined (Int 1) typeInt)

    it "int is not subsumed by string" $
      shouldFail (scheme typeInt `subsumedBy` Literal Predefined (String "foo") typeString)

    it "TFn of TVars is subsumed by same TFn" $
      let expr tv = Fn Predefined
                    (NameBinding Missing "x")
                    (Symbol Missing (Unqualified "x") tv) (typeFn tv tv) in
        scheme (typeFn tvarA tvarA) `subsumedBy` expr tvarB
        `shouldSucceedWith`
        (scheme (typeFn tvarA tvarA), expr tvarA)

    it "TFn of a tvars is not subsumed by TFn from string to int" $
      let expr = Fn Predefined
                    (NameBinding Missing "x")
                    (Symbol Missing (Unqualified "x") tvarA) (typeFn tvarA tvarA)
      in shouldFail (scheme (typeFn typeString typeInt) `subsumedBy` expr)

    it "TVar is not subsumed by TFn" $
      let expr = Symbol Missing (Unqualified "x") (typeFn tvarB tvarB) in
        shouldFail (scheme tvarA `subsumedBy` expr)

    it "tuple of tvars is subsumed by tuple of same tvars" $
      let tupleType = (TTuple Predefined tvarA tvarA [])
          expr = Symbol Missing (Unqualified "x") tupleType in
        scheme tupleType `subsumedBy` expr
        `shouldSucceedWith`
        (scheme tupleType, expr)

    it "tvar slice is subsumed by same tvar slice" $
      let expr tv = Slice Predefined [Symbol Missing (Unqualified "x") tv] (typeSlice tv) in
        scheme (typeSlice tvarA) `subsumedBy` expr tvarA
        `shouldSucceedWith`
        (scheme (typeSlice tvarA), expr tvarA)

    it "TNamed TFn of TVars is subsumed by unnamed but equal TFn" $
      let expr tv = Fn Predefined
                    (NameBinding Missing "x")
                    (Symbol Missing (Unqualified "x") tv) (typeFn tv tv) in
        scheme (named "MyFn" $ typeFn tvarA tvarA) `subsumedBy` expr tvarB
        `shouldSucceedWith`
        (scheme (named "MyFn" $ typeFn tvarA tvarA), expr tvarA)

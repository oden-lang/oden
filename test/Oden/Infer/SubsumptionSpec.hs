module Oden.Infer.SubsumptionSpec where

import           Test.Hspec

import           Oden.Infer.Subsumption
import           Oden.SourceInfo
import           Oden.Core
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.Type.Basic
import           Oden.Type.Polymorphic

import           Oden.Assertions


tvarA :: Type
tvarA = TVar (Metadata Predefined) (TV "a")

tvarB :: Type
tvarB = TVar (Metadata Predefined) (TV "b")

tvarC :: Type
tvarC = TVar (Metadata Predefined) (TV "c")

scheme:: Type -> Scheme
scheme= Forall (Metadata Predefined) []

typeUnit, typeAny, typeInt, typeString :: Type
typeUnit = TUnit (Metadata Predefined)
typeAny = TAny (Metadata Predefined)
typeInt = TBasic (Metadata Predefined) TInt
typeString = TBasic (Metadata Predefined) TString

typeFn :: Type -> Type -> Type
typeFn = TFn (Metadata Missing)

named :: String -> Type -> Type
named = TNamed (Metadata Missing) . FQN [] . Identifier

typeSlice :: Type -> Type
typeSlice = TSlice (Metadata Missing)

spec :: Spec
spec =
  describe "subsumedBy" $ do

    it "any is subsumed by any" $
      scheme typeAny `subsumedBy` Literal (Metadata Predefined) Unit typeAny
      `shouldSucceedWith`
      (scheme typeAny, Literal (Metadata Predefined) Unit typeAny)

    it "any is subsumed by int" $
      scheme typeAny `subsumedBy` Literal (Metadata Predefined) Unit typeUnit
      `shouldSucceedWith`
      (scheme typeAny, Literal (Metadata Predefined) Unit typeUnit)

    it "int is not subsumed by any" $
      shouldFail (scheme typeInt `subsumedBy` Literal (Metadata Predefined) Unit typeAny)

    it "tvar is not subsumed by any" $
      shouldFail (scheme tvarA `subsumedBy` Literal (Metadata Predefined) Unit typeAny)

    it "any is subsumed by tvar" $
      scheme typeAny `subsumedBy` Literal (Metadata Predefined) Unit tvarA
      `shouldSucceedWith`
      (scheme typeAny, Literal (Metadata Predefined) Unit typeAny)

    it "tvar is subsumed by same tvar" $
      scheme tvarA `subsumedBy` Literal (Metadata Predefined) Unit tvarA
      `shouldSucceedWith`
      (scheme tvarA, Literal (Metadata Predefined) Unit tvarA)

    it "tvar b is subsumed by tvar a and all a tvars are substituted" $
      scheme tvarB `subsumedBy` Literal (Metadata Predefined) Unit tvarA
      `shouldSucceedWith`
      (scheme tvarB, Literal (Metadata Predefined) Unit tvarB)

    it "int is subsumed by int" $
      scheme typeInt `subsumedBy` Literal (Metadata Predefined) (Int 1) typeInt
      `shouldSucceedWith`
      (scheme typeInt, Literal (Metadata Predefined) (Int 1) typeInt)

    it "string is subsumed by string" $
      scheme typeString `subsumedBy` Literal (Metadata Predefined) (String "foo") typeString
      `shouldSucceedWith`
      (scheme typeString, Literal (Metadata Predefined) (String "foo") typeString)

    it "string is not subsumed by int" $
      shouldFail (scheme typeString `subsumedBy` Literal (Metadata Predefined) (Int 1) typeInt)

    it "int is not subsumed by string" $
      shouldFail (scheme typeInt `subsumedBy` Literal (Metadata Predefined) (String "foo") typeString)

    it "TFn of TVars is subsumed by same TFn" $
      let expr tv = Fn (Metadata Predefined)
                    (NameBinding (Metadata Missing) (Identifier "x"))
                    (Symbol (Metadata Missing) (Identifier "x") tv) (typeFn tv tv) in
        scheme (typeFn tvarA tvarA) `subsumedBy` expr tvarB
        `shouldSucceedWith`
        (scheme (typeFn tvarA tvarA), expr tvarA)

    it "TFn of a tvars is not subsumed by TFn from string to int" $
      let expr = Fn (Metadata Predefined)
                    (NameBinding (Metadata Missing) (Identifier "x"))
                    (Symbol (Metadata Missing) (Identifier "x") tvarA) (typeFn tvarA tvarA)
      in shouldFail (scheme (typeFn typeString typeInt) `subsumedBy` expr)

    it "TVar is not subsumed by TFn" $
      let expr = Symbol (Metadata Missing) (Identifier "x") (typeFn tvarB tvarB) in
        shouldFail (scheme tvarA `subsumedBy` expr)

    it "tuple of tvars is subsumed by tuple of same tvars" $
      let tupleType = (TTuple (Metadata Predefined) tvarA tvarA [])
          expr = Symbol (Metadata Missing) (Identifier "x") tupleType in
        scheme tupleType `subsumedBy` expr
        `shouldSucceedWith`
        (scheme tupleType, expr)

    it "tvar slice is subsumed by same tvar slice" $
      let expr tv = Slice (Metadata Predefined) [Symbol (Metadata Missing) (Identifier "x") tv] (typeSlice tv) in
        scheme (typeSlice tvarA) `subsumedBy` expr tvarA
        `shouldSucceedWith`
        (scheme (typeSlice tvarA), expr tvarA)

    it "TNamed TFn of TVars is subsumed by unnamed but equal TFn" $
      let expr tv = Fn (Metadata Predefined)
                    (NameBinding (Metadata Missing) (Identifier "x"))
                    (Symbol (Metadata Missing) (Identifier "x") tv) (typeFn tv tv) in
        scheme (named "MyFn" $ typeFn tvarA tvarA) `subsumedBy` expr tvarB
        `shouldSucceedWith`
        (scheme (named "MyFn" $ typeFn tvarA tvarA), expr tvarA)

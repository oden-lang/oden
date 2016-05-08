module Oden.Infer.UnificationSpec where

import           Test.Hspec

import           Oden.SourceInfo
import           Oden.Identifier
import           Oden.Infer.Substitution
import           Oden.Infer.Unification
import           Oden.Predefined
import           Oden.Type.Polymorphic

import           Data.Map
import qualified Data.Set as Set

import           Oden.Assertions
import           Oden.Infer.Fixtures

unify :: Type -> Type -> Either UnificationError Subst
unify t1 t2 = runSolve [UnifyConstraint (getSourceInfo t1) t1 t2]

spec =
  describe "unify" $ do

    it "unifies int with int" $
      unify typeInt typeInt
      `shouldSucceedWith`
      emptySubst

    it "unifies string with string" $
      unify typeString typeString
      `shouldSucceedWith`
      emptySubst

    it "unifies a -> b with int -> string" $
      unify (typeFn tvarA tvarB) (typeFn typeInt typeString)
      `shouldSucceedWith`
      Subst (fromList [(TV "a", typeInt), (TV "b", typeString)])

    it "unifies []{a} with []{b}" $
      unify (TSlice missing tvarA) (TSlice missing tvarB)
      `shouldSucceedWith`
      Subst (singleton (TV "a") tvarB)

    it "unifies { foo: int } with a" $
      let oneFieldRow = RExtension missing (Identifier "foo") typeInt (REmpty missing) in
        unify oneFieldRow tvarA
        `shouldSucceedWith`
        Subst (singleton (TV "a") oneFieldRow)

    it "unifies { foo: int | b } with a" $
      let oneFieldRow = RExtension missing (Identifier "foo") typeInt (REmpty missing) in
        unify oneFieldRow tvarA
        `shouldSucceedWith`
        Subst (singleton (TV "a") oneFieldRow)

    it "does not unify { foo: int } with { bar: int }" $
      let fooRow = RExtension missing (Identifier "foo") typeInt (REmpty missing)
          barRow = RExtension missing (Identifier "bar") typeInt (REmpty missing) in
        shouldFail $ unify fooRow barRow

    it "unifies a with an empty row when unifying { foo: int | a } with { foo: int }" $
      let withRowVariable = RExtension missing (Identifier "foo") typeInt tvarA
          withoutRowVariable = RExtension missing (Identifier "foo") typeInt (REmpty missing) in
        unify withRowVariable withoutRowVariable
        `shouldSucceedWith`
        Subst (singleton (TV "a") (REmpty missing))

    it "unifies disregarding order of fields" $
      let firstRow = RExtension missing (Identifier "foo") typeInt (RExtension missing (Identifier "bar") typeInt (REmpty missing))
          secondRow = RExtension missing (Identifier "bar") typeInt (RExtension missing (Identifier "foo") typeInt (REmpty missing)) in
        unify firstRow secondRow
        `shouldSucceedWith`
        emptySubst

    it "unifies 'a' with a row including 'bar' when unifying { foo: int | a } with { bar: int, foo: int }" $
      let withRowVariable = RExtension missing (Identifier "foo") typeInt tvarA
          withTwoFields = RExtension missing (Identifier "bar") typeInt (RExtension missing (Identifier "foo") typeInt (REmpty missing)) in
        unify withRowVariable withTwoFields
        `shouldSucceedWith`
        Subst (singleton (TV "a") (RExtension missing (Identifier "bar") typeInt (REmpty missing)))

    it "unifies fns of records and fields from fn to tvar" $
      let rowFn = TFn missing (TRecord missing (RExtension missing (Identifier "bar") tvarA (TVar missing (TV "r")))) tvarA
          varFn = TFn missing tvarB tvarC in
      unify rowFn varFn
      `shouldSucceedWith`
      Subst (fromList [(TV "b", TRecord missing (RExtension missing (Identifier "bar") tvarC (TVar missing (TV "r")))),
                       (TV "a", tvarC)])

    it "unifies fns of records and fields from tvar to fn" $
      let rowFn = TFn missing (TRecord missing (RExtension missing (Identifier "bar") tvarA (TVar missing (TV "r")))) tvarA
          varFn = TFn missing tvarB tvarC in
      unify varFn rowFn
      `shouldSucceedWith`
      Subst (fromList [(TV "b", TRecord missing (RExtension missing (Identifier "bar") tvarA (TVar missing (TV "r")))),
                       (TV "c", tvarA)])

    it "unifies slices of functions from tvar to tvar" $
      unify (typeSlice (typeFn typeString typeString))
            (typeSlice (typeFn tvarA tvarB))
      `shouldSucceedWith`
      Subst (fromList [(TV "a", typeString), (TV "b", typeString)])

    it "unifies constrained type variables" $
      unify
      (TConstrained (Set.singleton $ ProtocolConstraint missing testableProtocolName tvarA) tvarA)
      (TConstrained (Set.singleton $ ProtocolConstraint missing testableProtocolName tvarB) tvarB)
      `shouldSucceedWith`
      Subst (singleton (TV "a") tvarB)

    it "unifies constrained functions" $
      unify (TConstrained Set.empty $ typeFn tvarA tvarB) (TConstrained Set.empty $ typeFn typeInt typeString)
      `shouldSucceedWith`
      Subst (fromList [(TV "a", typeInt), (TV "b", typeString)])

    it "unifies nested constrained functions" $
      unify
      (TConstrained Set.empty $ TConstrained Set.empty $ typeFn tvarA tvarB)
      (TConstrained Set.empty $ typeFn typeInt typeString)
      `shouldSucceedWith`
      Subst (fromList [(TV "a", typeInt), (TV "b", typeString)])

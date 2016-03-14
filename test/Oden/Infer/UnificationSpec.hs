module Oden.Infer.UnificationSpec where

import           Test.Hspec

import           Oden.SourceInfo
import           Oden.Identifier
import           Oden.Infer.Substitution
import           Oden.Infer.Unification
import           Oden.Predefined
import           Oden.Type.Polymorphic

import           Data.Map

import           Oden.Assertions
import           Oden.Infer.Fixtures

unify :: Type -> Type -> Either UnificationError Subst
unify t1 t2 = runSolve [(getSourceInfo t1, t1, t2)]

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

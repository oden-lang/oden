module Oden.Infer.SubsumptionSpec where

import           Data.Map
import           Test.Hspec

import           Oden.Infer.Subsumption
import           Oden.Type.Polymorphic
import           Oden.Infer.Substitution

import           Oden.Assertions


tvarA :: Type
tvarA = TVar (TV "a")

tvarB :: Type
tvarB = TVar (TV "b")

tvarC :: Type
tvarC = TVar (TV "c")

spec :: Spec
spec = do

  describe "getSubst" $ do
    it "gets substitutions for tuple" $
      getSubst
      (TTuple tvarA tvarB [])
      (TTuple tvarB tvarC [])
      `shouldSucceedWith`
      Subst (fromList [(TV "b", tvarA), (TV "c", tvarB)])
    it "gets substitutions for fn of tuple" $
      getSubst
      (TFn tvarA (TFn tvarB (TTuple tvarA tvarB [])))
      (TFn tvarB (TFn tvarC (TTuple tvarB tvarC [])))
      `shouldSucceedWith`
      Subst (fromList [(TV "b", tvarA), (TV "c", tvarB)])

  describe "subsume" $ do
    it "any subsume any" $
      TAny `subsume` TAny
      `shouldSucceedWith`
      TAny
    it "any subsume int" $
      TAny `subsume` typeInt
      `shouldSucceedWith`
      TAny
    it "int does not subsume any" $
      shouldFail (typeInt `subsume` TAny)
    it "tvar does not subsume any" $
      shouldFail (tvarA `subsume` TAny)
    it "any subsume tvar" $
      TAny `subsume` tvarA
      `shouldSucceedWith`
      TAny
    it "tvar subsume same tvar" $
      tvarA `subsume` tvarA
      `shouldSucceedWith`
      tvarA
    it "tvar does not subsume other tvars" $
      shouldFail (tvarA `subsume` tvarB)
    it "tcon subsume same tcon" $
      typeInt `subsume` typeInt
      `shouldSucceedWith`
      typeInt
    it "tcon does not subsume other tcons" $
      shouldFail (typeInt `subsume` typeBool)
    it "TFn of TVars subsume same TFn" $
      TFn tvarA tvarA `subsume` TFn tvarA tvarA
      `shouldSucceedWith`
      TFn tvarA tvarA
    it "TVar does not subsume TFn" $
      shouldFail (tvarA `subsume` TFn tvarB tvarB)
    it "tuple of tvars subsumes tuple of same tvars" $
      TTuple tvarA tvarA [] `subsume` TTuple tvarA tvarA []
      `shouldSucceedWith`
      TTuple tvarA tvarA []

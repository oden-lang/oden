module Oden.Infer.SubsumptionSpec where

import           Data.Map
import           Test.Hspec

import           Oden.Infer.Subsumption
import           Oden.Infer.Substitution
import           Oden.SourceInfo
import           Oden.Type.Basic
import           Oden.Type.Polymorphic

import           Oden.Assertions


tvarA :: Type
tvarA = TVar Predefined (TV "a")

tvarB :: Type
tvarB = TVar Predefined (TV "b")

tvarC :: Type
tvarC = TVar Predefined (TV "c")

spec :: Spec
spec = do

  describe "getSubst" $ do
    it "gets substitutions for tuple" $
      getSubst
      (TTuple Predefined tvarA tvarB [])
      (TTuple Predefined tvarB tvarC [])
      `shouldSucceedWith`
      Subst (fromList [(TV "b", tvarA), (TV "c", tvarB)])
    it "gets substitutions for fn of tuple" $
      getSubst
      (TFn Predefined tvarA (TFn Predefined tvarB (TTuple Predefined tvarA tvarB [])))
      (TFn Predefined tvarB (TFn Predefined tvarC (TTuple Predefined tvarB tvarC [])))
      `shouldSucceedWith`
      Subst (fromList [(TV "b", tvarA), (TV "c", tvarB)])

  describe "subsume" $ do
    it "any subsume any" $
      TAny Predefined `subsume` TAny Predefined
      `shouldSucceedWith`
      TAny Predefined
    it "any subsume int" $
      TAny Predefined `subsume` TBasic Predefined TInt
      `shouldSucceedWith`
      TAny Predefined
    it "int does not subsume any" $
      shouldFail (TBasic Predefined TInt `subsume` TAny Predefined)
    it "tvar does not subsume any" $
      shouldFail (tvarA `subsume` TAny Predefined)
    it "any subsume tvar" $
      TAny Predefined `subsume` tvarA
      `shouldSucceedWith`
      TAny Predefined
    it "tvar subsume same tvar" $
      tvarA `subsume` tvarA
      `shouldSucceedWith`
      tvarA
    it "tvar does not subsume other tvars" $
      shouldFail (tvarA `subsume` tvarB)
    it "int subsume same int" $
      TBasic Predefined TInt `subsume` TBasic Predefined TInt
      `shouldSucceedWith`
      TBasic Predefined TInt
    it "string subsume same string" $
      TBasic Predefined TString `subsume` TBasic Predefined TString
      `shouldSucceedWith`
      TBasic Predefined TString
    it "tcon subsume same tcon" $
      TCon Missing "foo" `subsume` TCon Missing "foo"
      `shouldSucceedWith`
      TCon Missing "foo"
    it "tcon does not subsume other tcons" $
      shouldFail (TBasic Predefined TInt `subsume` TBasic Predefined TBool)
    it "TFn of TVars subsume same TFn" $
      TFn Predefined tvarA tvarA `subsume` TFn Predefined tvarA tvarA
      `shouldSucceedWith`
      TFn Predefined tvarA tvarA
    it "TVar does not subsume TFn" $
      shouldFail (tvarA `subsume` TFn Predefined tvarB tvarB)
    it "tuple of tvars subsumes tuple of same tvars" $
      TTuple Predefined tvarA tvarA [] `subsume` TTuple Predefined tvarA tvarA []
      `shouldSucceedWith`
      TTuple Predefined tvarA tvarA []

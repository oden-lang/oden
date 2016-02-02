module Oden.Compiler.ValidationSpec where

import           Test.Hspec

import           Oden.Compiler.Validation
import           Oden.Core
import           Oden.Type.Polymorphic

import           Oden.Assertions

unitExpr :: CanonicalExpr
unitExpr = (Forall [] TUnit, Literal Unit TUnit)

spec :: Spec
spec =
  describe "checkRedefinitions" $ do
    it "accepts uniquely named definitions" $
      checkRedefinitions (Package ["mypkg"] [] [
            Definition "foo" unitExpr,
            Definition "bar" unitExpr,
            Definition "baz" unitExpr
        ])
      `shouldSucceedWith`
      []
    it "throws an error on duplicate top-level names" $
      checkRedefinitions (Package ["mypkg"] [] [
            Definition "foo" unitExpr,
            Definition "bar" unitExpr,
            Definition "foo" unitExpr
        ])
      `shouldFailWith`
      Redefinition "foo"

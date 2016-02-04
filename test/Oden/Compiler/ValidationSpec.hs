module Oden.Compiler.ValidationSpec where

import           Test.Hspec

import           Oden.Compiler.Validation
import           Oden.Core
import           Oden.Identifier
import           Oden.Type.Polymorphic

import           Oden.Assertions

canonical :: Expr Type -> CanonicalExpr
canonical e = (Forall [] (typeOf e), e)

unitExpr :: Expr Type
unitExpr = Literal Unit TUnit

strExpr :: Expr Type
strExpr = Literal (String "hello") (TCon "string")

letExpr :: Name -> Expr Type -> Expr Type -> Expr Type
letExpr n value body = Let n value body (typeOf body)

fnExpr :: Name -> Expr Type -> Expr Type
fnExpr n body = Fn n body (TFn (TCon "string") (typeOf body))

spec :: Spec
spec =
  describe "validate" $ do

    it "accepts uniquely named definitions" $
      validate (Package ["mypkg"] [] [
            Definition "foo" (canonical strExpr),
            Definition "bar" (canonical strExpr),
            Definition "baz" (canonical strExpr)
        ])
      `shouldSucceedWith`
      []

    it "throws an error on duplicate top-level names" $
      validate (Package ["mypkg"] [] [
            Definition "foo" $ canonical strExpr,
            Definition "bar" $ canonical strExpr,
            Definition "foo" $ canonical strExpr
        ])
      `shouldFailWith`
      Redefinition "foo"

    it "throws an error on let-bound name shadowing top-level definition" $
      validate (Package ["mypkg"] [] [
            Definition
            "foo"
            (canonical strExpr),
            Definition
            "bar"
            (canonical (letExpr "foo" strExpr strExpr))
        ])
      `shouldFailWith`
      Redefinition "foo"

    it "throws an error on let-bound name shadowing other let-bound name" $
      validate (Package ["mypkg"] [] [
            Definition
            "bar"
            (canonical (letExpr "foo" strExpr (letExpr "foo" strExpr strExpr)))
        ])
      `shouldFailWith`
      Redefinition "foo"

    it "throws an error on arg shadowing top-level definition" $
      validate (Package ["mypkg"] [] [
            Definition
            "foo"
            (canonical strExpr),
            Definition
            "bar"
            (canonical (fnExpr "foo" strExpr))
        ])
      `shouldFailWith`
      Redefinition "foo"

    it "throws an error on fn arg shadowing other fn arg" $
      validate (Package ["mypkg"] [] [
            Definition
            "bar"
            (canonical (fnExpr "foo" (fnExpr "foo" strExpr)))
        ])
      `shouldFailWith`
      Redefinition "foo"

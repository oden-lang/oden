module Oden.Compiler.ValidationSpec where

import           Test.Hspec

import           Oden.Compiler.Validation
import           Oden.Core
import           Oden.Identifier
import           Oden.SourceInfo
import           Oden.Type.Basic
import           Oden.Type.Polymorphic

import           Oden.Assertions

canonical :: Expr Type -> CanonicalExpr
canonical e = (Forall Missing [] (typeOf e), e)

unitExpr :: Expr Type
unitExpr = Literal Missing Unit (TUnit Missing)

strExpr :: Expr Type
strExpr = Literal Missing (String "hello") (TCon Missing "string")

letExpr :: Name -> Expr Type -> Expr Type -> Expr Type
letExpr n value body = Let Missing (Binding Missing n) value body (typeOf body)

fnExpr :: Name -> Expr Type -> Expr Type
fnExpr n body = Fn Missing (Binding Missing n) body (TFn Missing (TBasic Missing TString) (typeOf body))

spec :: Spec
spec =
  describe "validate" $ do

    it "accepts uniquely named definitions" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition Missing "foo" (canonical strExpr),
            Definition Missing "bar" (canonical strExpr),
            Definition Missing "baz" (canonical strExpr)
        ])
      `shouldSucceedWith`
      []

    it "throws an error on duplicate top-level names" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition Missing "foo" $ canonical strExpr,
            Definition Missing "bar" $ canonical strExpr,
            Definition Missing "foo" $ canonical strExpr
        ])
      `shouldFailWith`
      Redefinition Missing "foo"

    it "throws an error on let-bound name shadowing top-level definition" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition
            Missing
            "foo"
            (canonical strExpr),
            Definition
            Missing
            "bar"
            (canonical (letExpr "foo" strExpr strExpr))
        ])
      `shouldFailWith`
      Redefinition Missing "foo"

    it "throws an error on let-bound name shadowing other let-bound name" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition
            Missing
            "bar"
            (canonical (letExpr "foo" strExpr (letExpr "foo" strExpr strExpr)))
        ])
      `shouldFailWith`
      Redefinition Missing "foo"

    it "throws an error on arg shadowing top-level definition" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition
            Missing
            "foo"
            (canonical strExpr),
            Definition
            Missing
            "bar"
            (canonical (fnExpr "foo" strExpr))
        ])
      `shouldFailWith`
      Redefinition Missing "foo"

    it "throws an error on fn arg shadowing other fn arg" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition
            Missing
            "bar"
            (canonical (fnExpr "foo" (fnExpr "foo" strExpr)))
        ])
      `shouldFailWith`
      Redefinition Missing "foo"

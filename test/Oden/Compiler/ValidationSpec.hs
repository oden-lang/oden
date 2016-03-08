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
strExpr = Literal Missing (String "hello") (TBasic Missing TString)

letExpr :: Identifier -> Expr Type -> Expr Type -> Expr Type
letExpr n value body = Let Missing (NameBinding Missing n) value body (typeOf body)

fnExpr :: Identifier -> Expr Type -> Expr Type
fnExpr n body = Fn Missing (NameBinding Missing n) body (TFn Missing (TBasic Missing TString) (typeOf body))

block :: [Expr Type] -> Expr Type
block exprs = Block Missing exprs (typeOf (last exprs))

spec :: Spec
spec =
  describe "validate" $ do

    it "warns on discarded value in block" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition Missing (Identifier "foo") $ canonical (block [strExpr, unitExpr])
        ])
      `shouldFailWith`
      ValueDiscarded strExpr

    it "does not warn on discarded unit value in block" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition Missing (Identifier "foo") $ canonical (block [unitExpr, strExpr])
        ])
      `shouldSucceedWith`
      []

    it "accepts uniquely named definitions" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition Missing (Identifier "foo") (canonical strExpr),
            Definition Missing (Identifier "bar") (canonical strExpr),
            Definition Missing (Identifier "baz") (canonical strExpr)
        ])
      `shouldSucceedWith`
      []

    it "throws an error on duplicate top-level names" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition Missing (Identifier "foo") $ canonical strExpr,
            Definition Missing (Identifier "bar") $ canonical strExpr,
            Definition Missing (Identifier "foo") $ canonical strExpr
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on let-bound name shadowing top-level definition" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition
            Missing
            (Identifier "foo")
            (canonical strExpr),
            Definition
            Missing
            (Identifier "bar")
            (canonical (letExpr (Identifier "foo") strExpr strExpr))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on let-bound name shadowing other let-bound name" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition
            Missing
            (Identifier "bar")
            (canonical (letExpr (Identifier "foo") strExpr (letExpr (Identifier "foo") strExpr strExpr)))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on arg shadowing top-level definition" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition
            Missing
            (Identifier "foo")
            (canonical strExpr),
            Definition
            Missing
            (Identifier "bar")
            (canonical (fnExpr (Identifier "foo") strExpr))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on fn arg shadowing other fn arg" $
      validate (Package (PackageDeclaration Missing ["mypkg"]) [] [
            Definition
            Missing
            (Identifier "bar")
            (canonical (fnExpr (Identifier "foo") (fnExpr (Identifier "foo") strExpr)))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

module Oden.Compiler.ValidationSpec where

import           Test.Hspec

import           Oden.Compiler.Validation
import           Oden.Core
import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.Type.Basic
import           Oden.Type.Polymorphic

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

canonical :: Expr Type -> CanonicalExpr
canonical e = (Forall missing [] (typeOf e), e)

unitExpr :: Expr Type
unitExpr = Literal missing Unit (TUnit missing)

strExpr :: Expr Type
strExpr = Literal missing (String "hello") (TBasic missing TString)

letExpr :: Identifier -> Expr Type -> Expr Type -> Expr Type
letExpr n value body = Let missing (NameBinding missing n) value body (typeOf body)

fnExpr :: Identifier -> Expr Type -> Expr Type
fnExpr n body = Fn missing (NameBinding missing n) body (TFn missing (TBasic missing TString) (typeOf body))

block :: [Expr Type] -> Expr Type
block exprs = Block missing exprs (typeOf (last exprs))

spec :: Spec
spec =
  describe "validate" $ do

    it "warns on discarded value in block" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") $ canonical (block [strExpr, unitExpr])
        ])
      `shouldFailWith`
      ValueDiscarded strExpr

    it "does not warn on discarded unit value in block" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") $ canonical (block [unitExpr, strExpr])
        ])
      `shouldSucceedWith`
      []

    it "accepts uniquely named definitions" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") (canonical strExpr),
            Definition missing (Identifier "bar") (canonical strExpr),
            Definition missing (Identifier "baz") (canonical strExpr)
        ])
      `shouldSucceedWith`
      []

    it "throws an error on duplicate top-level names" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") $ canonical strExpr,
            Definition missing (Identifier "bar") $ canonical strExpr,
            Definition missing (Identifier "foo") $ canonical strExpr
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on let-bound name shadowing top-level definition" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            (canonical strExpr),
            Definition
            missing
            (Identifier "bar")
            (canonical (letExpr (Identifier "foo") strExpr strExpr))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on let-bound name shadowing other let-bound name" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "bar")
            (canonical (letExpr (Identifier "foo") strExpr (letExpr (Identifier "foo") strExpr strExpr)))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on arg shadowing top-level definition" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            (canonical strExpr),
            Definition
            missing
            (Identifier "bar")
            (canonical (fnExpr (Identifier "foo") strExpr))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on fn arg shadowing other fn arg" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "bar")
            (canonical (fnExpr (Identifier "foo") (fnExpr (Identifier "foo") strExpr)))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

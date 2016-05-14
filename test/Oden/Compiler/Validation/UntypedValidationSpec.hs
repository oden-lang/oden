module Oden.Compiler.Validation.UntypedValidationSpec where

import           Test.Hspec

import           Oden.Compiler.Validation.Untyped

import           Oden.Core.Untyped      as Untyped
import           Oden.Core.Expr
import           Oden.Core.Package

import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

unitExpr :: UntypedExpr
unitExpr = Literal missing Unit Untyped

strExpr :: UntypedExpr
strExpr = Literal missing (String "hello") Untyped

intExpr :: Integer -> UntypedExpr
intExpr n = Literal missing (Int n) Untyped


letExpr :: Identifier -> UntypedExpr -> UntypedExpr -> UntypedExpr
letExpr n value body = Let missing (NameBinding missing n) value body (typeOf body)

fnExpr :: Identifier -> UntypedExpr -> UntypedExpr
fnExpr n body = Fn missing (NameBinding missing n) body Untyped

block :: [UntypedExpr] -> UntypedExpr
block exprs = Block missing exprs (typeOf (last exprs))

emptyPkg = UntypedPackage (PackageDeclaration missing ["empty", "pkg"]) [] []

spec :: Spec
spec = do
  describe "validateExpr" $ do

    it "accepts uniquely named definitions" $
      validate (UntypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") Nothing strExpr,
            Definition missing (Identifier "bar") Nothing strExpr,
            Definition missing (Identifier "baz") Nothing strExpr
        ])
      `shouldSucceedWith`
      []

    it "throws an error on duplicate top-level names" $
      validate (UntypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") Nothing strExpr,
            Definition missing (Identifier "bar") Nothing strExpr,
            Definition missing (Identifier "foo") Nothing strExpr
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on let-bound name shadowing top-level definition" $
      validate (UntypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            Nothing
            strExpr,
            Definition
            missing
            (Identifier "bar")
            Nothing
            (letExpr (Identifier "foo") strExpr strExpr)
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on let-bound name shadowing other let-bound name" $
      validate (UntypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "bar")
            Nothing
            (letExpr (Identifier "foo") strExpr (letExpr (Identifier "foo") strExpr strExpr))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on arg shadowing top-level definition" $
      validate (UntypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            Nothing
            strExpr,
            Definition
            missing
            (Identifier "bar")
            Nothing
            (fnExpr (Identifier "foo") strExpr)
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on fn arg shadowing other fn arg" $
      validate (UntypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "bar")
            Nothing
            (fnExpr (Identifier "foo") (fnExpr (Identifier "foo") strExpr))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

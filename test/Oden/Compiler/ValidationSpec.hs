module Oden.Compiler.ValidationSpec where

import           Test.Hspec

import           Oden.Compiler.Validation
import           Oden.Core
import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

typeUnit, typeString, typeInt, typeIntSlice :: Type
typeUnit = TCon (Metadata Predefined) (FQN [] (Identifier "unit"))
typeString = TCon (Metadata Predefined) (FQN [] (Identifier "string"))
typeInt = TCon (Metadata Predefined) (FQN [] (Identifier "int"))
typeIntSlice = TSlice missing typeInt

canonical :: Expr Type -> CanonicalExpr
canonical e = (Forall missing [] (typeOf e), e)

unitExpr :: Expr Type
unitExpr = Literal missing Unit typeUnit

strExpr :: Expr Type
strExpr = Literal missing (String "hello") typeString

intExpr :: Integer -> Expr Type
intExpr n = Literal missing (Int n) typeInt


letExpr :: Identifier -> Expr Type -> Expr Type -> Expr Type
letExpr n value body = Let missing (NameBinding missing n) value body (typeOf body)

fnExpr :: Identifier -> Expr Type -> Expr Type
fnExpr n body = Fn missing (NameBinding missing n) body (TFn missing typeString (typeOf body))

block :: [Expr Type] -> Expr Type
block exprs = Block missing exprs (typeOf (last exprs))

divisionByZeroExpr = BinaryOp missing Divide (intExpr 1) (intExpr 0) typeInt

emptyPkg = Package (PackageDeclaration missing ["empty", "pkg"]) [] []

spec :: Spec
spec = do
  describe "validateExpr" $ do

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

    it "throws an error on literal division by zero" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            (canonical divisionByZeroExpr)
        ])
      `shouldFailWith`
      DivisionByZero divisionByZeroExpr

    it "throws an error on negative subscript" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            (canonical $
              Subscript missing
                (Symbol missing (Identifier "s") typeIntSlice)
                (intExpr (-1))
                typeInt)
        ])
      `shouldFailWith`
      NegativeSliceIndex (intExpr (-1))

    it "throws an error on subslice from negative index" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            (canonical $
              Subslice missing
                (Symbol missing (Identifier "s") typeIntSlice)
                (RangeFrom (intExpr (-1)))
                typeInt)
        ])
      `shouldFailWith`
      NegativeSliceIndex (intExpr (-1))

    it "throws an error on subslice to negative index" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            (canonical $
              Subslice missing
                (Symbol missing (Identifier "s") typeIntSlice)
                (RangeTo (intExpr (-1)))
                typeInt)
        ])
      `shouldFailWith`
      NegativeSliceIndex (intExpr (-1))

    it "throws an error on subslice from higher to lower index" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            (canonical $
              Subslice missing
                (Symbol missing (Identifier "s") typeIntSlice)
                (Range (intExpr 10) (intExpr 5))
                typeInt)
        ])
      `shouldFailWith`
      InvalidSubslice Missing (Range (intExpr 10) (intExpr 5))

  describe "validatePackage" $ do

    it "throws an error on unused imports" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [
          ImportedPackage missing (Identifier "foo") emptyPkg
        ] [])
      `shouldFailWith`
      UnusedImport Missing ["empty", "pkg"] (Identifier "foo")

    it "does not throw errors for used imports" $
      validate (Package (PackageDeclaration missing ["mypkg"]) [
          ImportedPackage missing (Identifier "other") emptyPkg
        ] [
            Definition
            missing
            (Identifier "foo")
            (canonical (PackageMemberAccess missing (Identifier "other") (Identifier "s") typeInt))
        ])
      `shouldSucceedWith`
      []

    it "throws an error on duplicate imports" $ pending

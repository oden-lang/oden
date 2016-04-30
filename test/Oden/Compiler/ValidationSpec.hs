module Oden.Compiler.ValidationSpec where

import           Test.Hspec

import           Oden.Compiler.Validation

import           Oden.Core as Core
import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Operator
import           Oden.Core.Package
import           Oden.Core.Resolved

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import qualified Data.Set as Set

import           Oden.Assertions

missing :: Metadata SourceInfo
missing = Metadata Missing

typeUnit, typeString, typeInt, typeIntSlice :: Type
typeUnit = TCon (Metadata Predefined) (FQN [] (Identifier "unit"))
typeString = TCon (Metadata Predefined) (FQN [] (Identifier "string"))
typeInt = TCon (Metadata Predefined) (FQN [] (Identifier "int"))
typeIntSlice = TSlice missing typeInt

canonical :: TypedExpr -> CanonicalExpr
canonical e = (Forall missing [] Set.empty (typeOf e), e)

unitExpr :: TypedExpr
unitExpr = Literal missing Unit typeUnit

strExpr :: TypedExpr
strExpr = Literal missing (String "hello") typeString

intExpr :: Integer -> TypedExpr
intExpr n = Literal missing (Int n) typeInt


letExpr :: Identifier -> TypedExpr -> TypedExpr -> TypedExpr
letExpr n value body = Let missing (NameBinding missing n) value body (typeOf body)

fnExpr :: Identifier -> TypedExpr -> TypedExpr
fnExpr n body = Fn missing (NameBinding missing n) body (TFn missing typeString (typeOf body))

block :: [TypedExpr] -> TypedExpr
block exprs = Block missing exprs (typeOf (last exprs))

divisionByZeroExpr = BinaryOp missing Divide (intExpr 1) (intExpr 0) typeInt

emptyPkg = ResolvedPackage (PackageDeclaration missing ["empty", "pkg"]) [] []

spec :: Spec
spec = do
  describe "validateExpr" $ do

    it "warns on discarded value in block" $
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") $ canonical (block [strExpr, unitExpr])
        ])
      `shouldFailWith`
      ValueDiscarded strExpr

    it "does not warn on discarded unit value in block" $
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") $ canonical (block [unitExpr, strExpr])
        ])
      `shouldSucceedWith`
      []

    it "accepts uniquely named definitions" $
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") (canonical strExpr),
            Definition missing (Identifier "bar") (canonical strExpr),
            Definition missing (Identifier "baz") (canonical strExpr)
        ])
      `shouldSucceedWith`
      []

    it "throws an error on duplicate top-level names" $
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition missing (Identifier "foo") $ canonical strExpr,
            Definition missing (Identifier "bar") $ canonical strExpr,
            Definition missing (Identifier "foo") $ canonical strExpr
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on let-bound name shadowing top-level definition" $
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
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
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "bar")
            (canonical (letExpr (Identifier "foo") strExpr (letExpr (Identifier "foo") strExpr strExpr)))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on arg shadowing top-level definition" $
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
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
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "bar")
            (canonical (fnExpr (Identifier "foo") (fnExpr (Identifier "foo") strExpr)))
        ])
      `shouldFailWith`
      Redefinition Missing (Identifier "foo")

    it "throws an error on literal division by zero" $
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
            Definition
            missing
            (Identifier "foo")
            (canonical divisionByZeroExpr)
        ])
      `shouldFailWith`
      DivisionByZero divisionByZeroExpr

    it "throws an error on negative subscript" $
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
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
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
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
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
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
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [] [
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
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [
          ImportedPackage missing (Identifier "foo") emptyPkg
        ] [])
      `shouldFailWith`
      UnusedImport Missing ["empty", "pkg"] (Identifier "foo")

    it "does not throw errors for used imports" $
      validate (TypedPackage (PackageDeclaration missing ["mypkg"]) [
          ImportedPackage missing (Identifier "other") emptyPkg
        ] [
            Definition
            missing
            (Identifier "foo")
            (canonical
             (MemberAccess
              missing
              (Core.PackageMemberAccess (Identifier "other") (Identifier "s"))
              typeInt))
        ])
      `shouldSucceedWith`
      []

    it "throws an error on duplicate imports" $ pending

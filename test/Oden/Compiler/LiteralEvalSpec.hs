module Oden.Compiler.LiteralEvalSpec where

import           Test.Hspec

import           Oden.Compiler.LiteralEval
import           Oden.Core.Typed
import           Oden.Core.Expr
import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.Metadata
import           Oden.Predefined
import           Oden.SourceInfo

missing :: Metadata SourceInfo
missing = Metadata Missing

int :: Integer -> TypedExpr
int n = Literal missing (Int n) typeInt

true :: TypedExpr
true = Literal missing (Bool True) typeBool

false :: TypedExpr
false = Literal missing (Bool False) typeBool

intExpr :: BinaryOperator -> TypedExpr -> TypedExpr -> TypedExpr
intExpr op l r = BinaryOp missing op l r typeInt

boolExpr :: BinaryOperator -> TypedExpr -> TypedExpr -> TypedExpr
boolExpr op l r = BinaryOp missing op l r typeInt

spec :: Spec
spec =
  describe "evaluate" $ do

    it "evaluates integer literals" $
      evaluate (int 5) `shouldBe` Just (Int 5)

    it "evaluates addition" $
      evaluate (intExpr Add (int 2) (int 3)) `shouldBe` Just (Int 5)

    it "evaluates nested binary expressions: (3-2)*4 + 10/2" $
      evaluate (intExpr Add
                        (intExpr Multiply
                                 (intExpr Subtract
                                          (int 3)
                                          (int 2))
                                 (int 4))
                        (intExpr Divide (int 10) (int 2)))
      `shouldBe`
      Just (Int 9)

    it "evaluates boolean literals" $
      evaluate true `shouldBe` Just (Bool True)

    it "evaluates boolean expression: (true or false) and true" $
      evaluate (boolExpr And (boolExpr Or true false)
                              true)
      `shouldBe`
      Just (Bool True)

    it "evaluates integer comparison: (3 < 2) and (10 > 8)" $
      evaluate (boolExpr And
                        (boolExpr LessThan (int 3) (int 5))
                        (boolExpr GreaterThan (int 10) (int 8)))
      `shouldBe`
      Just (Bool True)

    it "evaluates if-expressions: if 2 == 3 then 1 else 2" $
      evaluate (If missing (boolExpr Equals (int 2) (int 3))
                           (int 1)
                           (int 2)
                   typeInt)
      `shouldBe`
      Just (Int 2)

    it "evaluates expressions with variables to Nothing" $
      evaluate (intExpr Add (int 5)
                            (Symbol missing (Identifier "x") typeInt))
      `shouldBe`
      Nothing

{-# LANGUAGE OverloadedStrings #-}
module Oden.SyntaxSpec where

import Test.Hspec

import Oden.Identifier
import Oden.Syntax
import qualified Oden.Core.Untyped as U

spec :: Spec
spec = do
  describe "explodeExpr" $ do
    it "converts symbol" $
      explodeExpr (Symbol (Unqualified "x"))
      `shouldBe`
      U.Symbol (Unqualified "x")

    it "converts int literal" $
      explodeExpr (Literal (Int 1))
      `shouldBe`
      U.Literal (U.Int 1)

    it "converts bool literal" $
      explodeExpr (Literal (Bool True))
      `shouldBe`
      U.Literal (U.Bool True)

    it "converts fn application with no params" $
      explodeExpr (Application (Fn [] (Symbol (Unqualified "x"))) [])
      `shouldBe`
      U.Application (U.NoArgFn (U.Symbol (Unqualified "x"))) []

    it "converts fn application with multiple params" $
      explodeExpr (Application (Fn ["x", "y"] (Symbol (Unqualified "x"))) [Symbol (Unqualified "x"), Symbol (Unqualified "y")])
      `shouldBe`
      U.Application (U.Fn "x" (U.Fn "y" (U.Symbol (Unqualified "x")))) [U.Symbol (Unqualified "x"), U.Symbol (Unqualified "y")]

  describe "explodeDefinition" $ do
    it "converts fn definition with no argument" $
      explodeDefinition (FnDefinition "f" [] (Symbol (Unqualified "x")))
      `shouldBe`
      U.Definition "f" (U.NoArgFn (U.Symbol (Unqualified "x")))

    it "converts fn definition with single argument" $
      explodeDefinition (FnDefinition "f" ["x"] (Symbol (Unqualified "x")))
      `shouldBe`
      U.Definition "f" (U.Fn "x" (U.Symbol (Unqualified "x")))

    it "converts fn definition with multiple arguments" $
      explodeDefinition (FnDefinition "f" ["x", "y"] (Symbol (Unqualified "x")))
      `shouldBe`
      U.Definition "f" (U.Fn "x" (U.Fn "y" (U.Symbol (Unqualified "x"))))

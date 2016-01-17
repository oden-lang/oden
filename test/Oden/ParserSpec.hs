{-# LANGUAGE OverloadedStrings #-}
module Oden.ParserSpec where

import           Test.Hspec

import           Oden.Identifier
import           Oden.Parser
import           Oden.Syntax

import           Oden.Assertions

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "parses + identifier" $
      parseExpr "+"
      `shouldSucceedWith`
      Symbol (Unqualified "+")

    it "parses +foo identifier" $
      parseExpr "+foo"
      `shouldSucceedWith`
      Symbol (Unqualified "+foo")

    it "parses qualified identifier" $
      parseExpr "foo.bar"
      `shouldSucceedWith`
      Symbol (Qualified "foo" "bar")

    it "parses integer literal" $
      parseExpr "123"
      `shouldSucceedWith`
      Literal (Int 123)

    it "parses +123 integer literal" $
      parseExpr "+123"
      `shouldSucceedWith`
      Literal (Int 123)

    it "parses false literal" $
      parseExpr "false"
      `shouldSucceedWith`
      Literal (Bool False)

    it "parses true literal" $
      parseExpr "true"
      `shouldSucceedWith`
      Literal (Bool True)

    it "parses string literal" $
      parseExpr "\"foo bar 123\""
      `shouldSucceedWith`
      Literal (String "foo bar 123")

    it "parses fn expression" $
      parseExpr "(fn (x) x)"
      `shouldSucceedWith`
      Fn ["x"] (Symbol (Unqualified "x"))

    it "parses multi-arg fn expression" $
      parseExpr "(fn (x y z) x)"
      `shouldSucceedWith`
      Fn ["x", "y", "z"] (Symbol (Unqualified "x"))

    it "parses no-arg fn expression" $
      parseExpr "(fn () x)"
      `shouldSucceedWith`
      Fn [] (Symbol (Unqualified "x"))

    it "parses if expression" $
      parseExpr "(if a b c)"
      `shouldSucceedWith`
      If (Symbol (Unqualified "a")) (Symbol (Unqualified "b")) (Symbol (Unqualified "c"))

    it "parses let expression" $
      parseExpr "(let ((x y)) z)"
      `shouldSucceedWith`
      Let [("x", Symbol (Unqualified "y"))] (Symbol (Unqualified "z"))

    it "parses single-arg fn application" $
      parseExpr "(x y)"
      `shouldSucceedWith`
      Application (Symbol (Unqualified "x")) [Symbol (Unqualified "y")]

    it "ignores whitespace" $
      parseExpr "(  x \n\n y \r\n\t   )"
      `shouldSucceedWith`
      Application (Symbol (Unqualified "x")) [Symbol (Unqualified "y")]

    it "ignores comments" $
      parseExpr "\n;; foobar\nx"
      `shouldSucceedWith`
      Symbol (Unqualified "x")

    it "ignores multi-line comments" $
      parseExpr "#;\n\n foo ;whatever\tbar\n;#x"
      `shouldSucceedWith`
      Symbol (Unqualified "x")

    it "parses slice literal" $
      parseExpr "![x y z]"
      `shouldSucceedWith`
      Slice [Symbol (Unqualified "x"), Symbol (Unqualified "y"), Symbol (Unqualified "z")]

  describe "parseDefinition" $ do
    it "parses value definition" $
      parseDefinition "(def x y)"
      `shouldSucceedWith`
      ValueDefinition "x" (Symbol (Unqualified "y"))

    it "parses fn definition" $
      parseDefinition "(def f (fn (x) x))"
      `shouldSucceedWith`
      ValueDefinition "f" (Fn ["x"] (Symbol (Unqualified "x")))

    it "parses short-hand fn definition" $
      parseDefinition "(def (f x) x)"
      `shouldSucceedWith`
      FnDefinition "f" ["x"] (Symbol (Unqualified "x"))

    it "parses short-hand no-arg fn definition" $
      parseDefinition "(def (side-effect) x)"
      `shouldSucceedWith`
      FnDefinition "side-effect" [] (Symbol (Unqualified "x"))

    it "parses short-hand multi-arg fn definition" $
      parseDefinition "(def (f x y z) x)"
      `shouldSucceedWith`
      FnDefinition "f" ["x", "y", "z"] (Symbol (Unqualified "x"))

  describe "parsePackage" $ do
    it "parses package declaration" $
      parsePackage "bar.oden" "(pkg foo/bar)"
      `shouldSucceedWith`
      Package ["foo", "bar"] [] []

    it "parses imports" $
      parsePackage "bar.oden" "(pkg foo/bar) (import bar/baz)\n(import foo)\n(import github.com/foo/bar)"
      `shouldSucceedWith`
      Package ["foo", "bar"] [Import ["bar", "baz"], Import ["foo"], Import ["github.com", "foo", "bar"]] []

    it "parses github.com import" $
      parsePackage "bar.oden" "(pkg foo/bar) (import github.com/foo/bar)"
      `shouldSucceedWith`
      Package ["foo", "bar"] [Import ["github.com", "foo", "bar"]] []

    it "parses definitions" $
      parsePackage "bar.oden" "(pkg foo/bar) (import bar/baz)"
      `shouldSucceedWith`
      Package ["foo", "bar"] [Import ["bar", "baz"]] []

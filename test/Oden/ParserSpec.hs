{-# LANGUAGE OverloadedStrings #-}
module Oden.ParserSpec where

import           Test.Hspec

import           Oden.Identifier
import           Oden.Parser
import           Oden.Syntax
import           Oden.Core.Operator

import           Oden.Assertions

typeInt :: TypeExpr
typeInt = TECon "int"

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

    it "parses + 123 as symbol and integer literal" $
      shouldFail $
        parseExpr "+ 123"

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
      parseExpr "fn x -> x"
      `shouldSucceedWith`
      Fn ["x"] (Symbol (Unqualified "x"))

    it "parses multi-arg fn expression" $
      parseExpr "fn x y z -> x"
      `shouldSucceedWith`
      Fn ["x", "y", "z"] (Symbol (Unqualified "x"))

    it "parses no-arg fn expression" $
      parseExpr "fn -> x"
      `shouldSucceedWith`
      Fn [] (Symbol (Unqualified "x"))

    it "parses if expression" $
      parseExpr "if a then b else c"
      `shouldSucceedWith`
      If (Symbol (Unqualified "a")) (Symbol (Unqualified "b")) (Symbol (Unqualified "c"))

    it "parses empty block as unit literal" $
      parseExpr "()"
      `shouldSucceedWith`
      Literal Unit

    it "parses expression in parens" $
      parseExpr "(())"
      `shouldSucceedWith`
      Literal Unit

    it "parses tuple with two elements" $
      parseExpr "(1, ())"
      `shouldSucceedWith`
      Tuple (Literal (Int 1)) (Literal Unit) []

    it "parses tuple with three elements" $
      parseExpr "(1, (), 2)"
      `shouldSucceedWith`
      Tuple (Literal (Int 1)) (Literal Unit) [Literal (Int 2)]

    it "parses block of symbols" $
      parseExpr "{ x\ny\nz }"
      `shouldSucceedWith`
      Block [
          Symbol (Unqualified "x"),
          Symbol (Unqualified "y"),
          Symbol (Unqualified "z")
        ]

    it "parses let binding and block of symbol" $
      parseExpr "let x = y in { x }"
      `shouldSucceedWith`
      Let [("x", Symbol (Unqualified "y"))] (Block [Symbol (Unqualified "x")])

    it "parses block with let binding and block of symbol" $
      parseExpr "{\nlet x = y in {\nx\n}\n}"
      `shouldSucceedWith`
      Block [Let [("x", Symbol (Unqualified "y"))] (Block [Symbol (Unqualified "x")])]

    it "fails on if expression with newlines" pending
    it "fails on let expression with newlines" pending

    it "parses let expression" $
      parseExpr "let x = y in z"
      `shouldSucceedWith`
      Let [("x", Symbol (Unqualified "y"))] (Symbol (Unqualified "z"))

    it "parses binary operator application" $
      parseExpr "x + y"
      `shouldSucceedWith`
      Op Add (Symbol (Unqualified "x")) (Symbol (Unqualified "y"))

    it "parses string concat application" $
      parseExpr "x ++ y"
      `shouldSucceedWith`
      Op Concat (Symbol (Unqualified "x")) (Symbol (Unqualified "y"))

    it "parses single-arg fn application" $
      parseExpr "x(y)"
      `shouldSucceedWith`
      Application (Symbol (Unqualified "x")) [Symbol (Unqualified "y")]

    it "parses single-arg fn application" $
      parseExpr "(fn x -> x)(y)"
      `shouldSucceedWith`
      Application (Fn ["x"] (Symbol (Unqualified "x"))) [Symbol (Unqualified "y")]

    it "ignores whitespace" $
      parseExpr "x(   \n\n y \r\n\t   )"
      `shouldSucceedWith`
      Application (Symbol (Unqualified "x")) [Symbol (Unqualified "y")]

    it "ignores comments" $
      parseExpr "\n// foobar\nx"
      `shouldSucceedWith`
      Symbol (Unqualified "x")

    it "ignores multi-line comments" $
      parseExpr "/*\n\n foo //whatever\tbar\n*/x"
      `shouldSucceedWith`
      Symbol (Unqualified "x")

    it "parses slice literal" $
      parseExpr "![x, y, z]"
      `shouldSucceedWith`
      Slice [Symbol (Unqualified "x"), Symbol (Unqualified "y"), Symbol (Unqualified "z")]

  describe "parseTopLevel" $ do
    it "parses type signature" $
      parseTopLevel "x :: int"
      `shouldSucceedWith`
      TypeSignature "x" (Implicit typeInt)

    it "parses type signature without explicit forall" $
      parseTopLevel "x :: int -> int"
      `shouldSucceedWith`
      TypeSignature "x" (Implicit (TEFn typeInt [typeInt]))

    it "parses type signature with no-arg fn" $
      parseTopLevel "x :: -> ()"
      `shouldSucceedWith`
      TypeSignature "x" (Implicit (TENoArgFn TEUnit))

    it "parses type signature with slice" $
      parseTopLevel "x :: ![int]"
      `shouldSucceedWith`
      TypeSignature "x" (Implicit (TESlice typeInt))

    it "parses polymorphic type signature with implicit forall" $
      parseTopLevel "x :: #a -> #a"
      `shouldSucceedWith`
      TypeSignature "x" (Implicit (TEFn (TEVar "a") [TEVar "a"]))

    it "parses polymorphic type signature with explicit forall" $
      parseTopLevel "x :: forall #a. #a -> #a"
      `shouldSucceedWith`
      TypeSignature "x" (Explicit ["a"] (TEFn (TEVar "a") [TEVar "a"]))

    it "parses polymorphic type signature" $
      parseTopLevel "x :: forall #a. #a -> #a"
      `shouldSucceedWith`
      TypeSignature "x" (Explicit ["a"] (TEFn (TEVar "a") [TEVar "a"]))

    it "parses value definition" $
      parseTopLevel "x = y"
      `shouldSucceedWith`
      ValueDefinition "x" (Symbol (Unqualified "y"))

    it "parses fn definition" $
      parseTopLevel "f = fn x -> x"
      `shouldSucceedWith`
      ValueDefinition "f" (Fn ["x"] (Symbol (Unqualified "x")))

    it "parses short-hand fn definition" $
      parseTopLevel "f x -> x"
      `shouldSucceedWith`
      FnDefinition "f" ["x"] (Symbol (Unqualified "x"))

    it "parses short-hand no-arg fn definition" $
      parseTopLevel "side-effect -> x"
      `shouldSucceedWith`
      FnDefinition "side-effect" [] (Symbol (Unqualified "x"))

    it "parses short-hand multi-arg fn definition" $
      parseTopLevel "f x y z -> x"
      `shouldSucceedWith`
      FnDefinition "f" ["x", "y", "z"] (Symbol (Unqualified "x"))

  describe "parsePackage" $ do
    it "parses package declaration" $
      parsePackage "bar.oden" "package foo/bar"
      `shouldSucceedWith`
      Package ["foo", "bar"] []

    it "parses imports" $
      parsePackage "bar.oden" "package foo/bar\nimport bar/baz\nimport foo\nimport github.com/foo/bar"
      `shouldSucceedWith`
      Package ["foo", "bar"] [ImportDeclaration ["bar", "baz"], ImportDeclaration ["foo"], ImportDeclaration ["github.com", "foo", "bar"]]

    it "parses github.com import" $
      parsePackage "bar.oden" "package foo/bar\nimport github.com/foo/bar"
      `shouldSucceedWith`
      Package ["foo", "bar"] [ImportDeclaration ["github.com", "foo", "bar"]]

    it "parses definitions" $
      parsePackage "bar.oden" "package foo/bar\nimport bar/baz"
      `shouldSucceedWith`
      Package ["foo", "bar"] [ImportDeclaration ["bar", "baz"]]

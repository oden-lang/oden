{-# LANGUAGE OverloadedStrings #-}
module Oden.Parser.ParseExprSpec where

import           Test.Hspec

import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.Parser
import           Oden.SourceInfo
import           Oden.Syntax

import           Oden.Assertions

src :: Line -> Column -> SourceInfo
src l c = SourceInfo (Position "<stdin>" l c)

spec :: Spec
spec = describe "parseExpr" $ do
  it "parses identifier" $
    parseExpr "foo"
    `shouldSucceedWith`
    Symbol (src 1 1) (Identifier "foo")

  it "parses single member access" $
    parseExpr "foo.bar"
    `shouldSucceedWith`
    MemberAccess
    (src 1 1)
    (Symbol (src 1 1) (Identifier "foo"))
    (Symbol (src 1 5) (Identifier "bar"))

  it "parses multiple member accesses" $
    parseExpr "foo.bar.baz"
    `shouldSucceedWith`
    MemberAccess
    (src 1 1)
    (MemberAccess
      (src 1 1)
      (Symbol (src 1 1) (Identifier "foo"))
      (Symbol (src 1 5) (Identifier "bar")))
    (Symbol (src 1 9) (Identifier "baz"))

  it "parses integer literal" $
    parseExpr "123"
    `shouldSucceedWith`
    Literal (src 1 1) (Int 123)

  it "parses false literal" $
    parseExpr "false"
    `shouldSucceedWith`
    Literal (src 1 1) (Bool False)

  it "parses true literal" $
    parseExpr "true"
    `shouldSucceedWith`
    Literal (src 1 1) (Bool True)

  it "parses string literal" $
    parseExpr "\"foo bar 123\""
    `shouldSucceedWith`
    Literal (src 1 1) (String "foo bar 123")

  it "parses fn expression" $
    parseExpr "(x) -> x"
    `shouldSucceedWith`
    Fn (src 1 1) [NameBinding (src 1 2) (Identifier "x")] (Symbol (src 1 8) (Identifier "x"))

  it "parses multi-arg fn expression" $
    parseExpr "(x, y, z) -> x"
    `shouldSucceedWith`
    Fn
    (src 1 1)
    [NameBinding (src 1 2) (Identifier "x"), NameBinding (src 1 5) (Identifier "y"), NameBinding (src 1 8) (Identifier "z")]
    (Symbol (src 1 14) (Identifier "x"))

  it "parses no-arg fn expression" $
    parseExpr "() -> x"
    `shouldSucceedWith`
    Fn (src 1 1) [] (Symbol (src 1 7) (Identifier "x"))

  it "parses if expression" $
    parseExpr "if a then b else c"
    `shouldSucceedWith`
    If
    (src 1 1)
    (Symbol (src 1 4) (Identifier "a"))
    (Symbol (src 1 11) (Identifier "b"))
    (Symbol (src 1 18) (Identifier "c"))

  it "parses empty block as unit literal" $
    parseExpr "()"
    `shouldSucceedWith`
    Literal (src 1 1) Unit

  it "parses expression in parens" $
    parseExpr "(())"
    `shouldSucceedWith`
    Literal (src 1 2) Unit

  it "parses tuple with two elements" $
    parseExpr "(1, ())"
    `shouldSucceedWith`
    Tuple
    (src 1 1)
    (Literal (src 1 2) (Int 1))
    (Literal (src 1 5) Unit)
    []

  it "parses tuple with three elements" $
    parseExpr "(1, (), 2)"
    `shouldSucceedWith`
    Tuple
    (src 1 1)
    (Literal (src 1 2) (Int 1))
    (Literal (src 1 5) Unit)
    [Literal (src 1 9) (Int 2)]

  it "parses block of symbols" $
    parseExpr "{\n  x\n  y\n  z\n}"
    `shouldSucceedWith`
    Block (src 1 1) [
        Symbol (src 2 3) (Identifier "x"),
        Symbol (src 3 3) (Identifier "y"),
        Symbol (src 4 3) (Identifier "z")
      ]

  it "parses let binding and block of symbol" $
    parseExpr "let x = y in { x }"
    `shouldSucceedWith`
    Let
    (src 1 1)
    [LetPair (src 1 5) (NameBinding (src 1 5) (Identifier "x")) (Symbol (src 1 9) (Identifier "y"))]
    (Block (src 1 14) [Symbol (src 1 16) (Identifier "x")])

  it "parses block with let binding and block of symbol" $
    parseExpr "{\n  let x = y in {\n    x\n  }\n}"
    `shouldSucceedWith`
    Block (src 1 1) [
        Let
        (src 2 3)
        [LetPair (src 2 7) (NameBinding (src 2 7) (Identifier "x")) (Symbol (src 2 11) (Identifier "y"))]
        (Block (src 2 16) [Symbol (src 3 5) (Identifier "x")])
      ]

  it "fails on if expression with newlines" pending
  it "fails on let expression with newlines" pending

  it "parses let expression" $
    parseExpr "let x = y in z"
    `shouldSucceedWith`
    Let
    (src 1 1)
    [LetPair (src 1 5) (NameBinding (src 1 5) (Identifier "x")) (Symbol (src 1 9) (Identifier "y"))]
    (Symbol (src 1 14) (Identifier "z"))

  it "parses unary negative operator application" $
    parseExpr "-x"
    `shouldSucceedWith`
    UnaryOp
    (src 1 1)
    Negative
    (Symbol (src 1 2) (Identifier "x"))

  it "parses unary positve operator application" $
    parseExpr "+ x"
    `shouldSucceedWith`
    UnaryOp
    (src 1 1)
    Positive
    (Symbol (src 1 3) (Identifier "x"))

  it "parses binary operator application" $
    parseExpr "x + y"
    `shouldSucceedWith`
    BinaryOp
    (src 1 3)
    Add
    (Symbol (src 1 1) (Identifier "x"))
    (Symbol (src 1 5) (Identifier "y"))

  it "parses string concat application" $
    parseExpr "x ++ y"
    `shouldSucceedWith`
    BinaryOp
    (src 1 3)
    Concat
    (Symbol (src 1 1) (Identifier "x"))
    (Symbol (src 1 6) (Identifier "y"))

  it "parses fn application with string concatenation" $
    parseExpr "foo(y) ++ \"!\""
    `shouldSucceedWith`
    BinaryOp
    (src 1 8)
    Concat
    (Application
      (src 1 4)
      (Symbol (src 1 1) (Identifier "foo"))
      [Symbol (src 1 5) (Identifier "y")])
    (Literal (src 1 11) (String "!"))

  it "parses package member fn application" $
    parseExpr "foo.Bar(y)"
    `shouldSucceedWith`
    Application
    (src 1 8)
    (MemberAccess
      (src 1 1)
      (Symbol (src 1 1) (Identifier "foo"))
      (Symbol (src 1 5) (Identifier "Bar")))
    [Symbol (src 1 9) (Identifier "y")]

  it "parses fn application member access" $
    pending
    {-
    parseExpr "foo(y).Bar"
    `shouldSucceedWith`
    MemberAccess
    (src 1 8)
    (Application
      (src 1 1)
      (Symbol (src 1 1) (Identifier "foo"))
      [Symbol (src 1 5) (Identifier "y")])
    (Symbol (src 1 8) (Identifier "Bar"))
    -}

  it "parses single-arg fn application" $
    parseExpr "x(y)"
    `shouldSucceedWith`
    Application
    (src 1 2)
    (Symbol (src 1 1) (Identifier "x"))
    [Symbol (src 1 3) (Identifier "y")]

  it "parses single-arg fn application" $
    parseExpr "((x) -> x)(y)"
    `shouldSucceedWith`
    Application
    (src 1 11)
    (Fn (src 1 2) [NameBinding (src 1 3) (Identifier "x")] (Symbol (src 1 9) (Identifier "x")))
    [Symbol (src 1 12) (Identifier "y")]

  it "ignores whitespace" $
    parseExpr "x(   \n\n y \r\n\t   )"
    `shouldSucceedWith`
    Application
    (src 1 2)
    (Symbol (src 1 1) (Identifier "x"))
    [Symbol (src 3 2) (Identifier "y")]

  it "ignores comments" $
    parseExpr "\n// foobar\nx"
    `shouldSucceedWith`
    Symbol (src 3 1) (Identifier "x")

  it "ignores multi-line comments" $
    parseExpr "/*\n\n foo //whatever\tbar\n*/x"
    `shouldSucceedWith`
    Symbol (src 4 3) (Identifier "x")

  it "parses slice literal" $
    parseExpr "[]{x, y, z}"
    `shouldSucceedWith`
    Slice (src 1 1) [
        Symbol (src 1 4) (Identifier "x"),
        Symbol (src 1 7) (Identifier "y"),
        Symbol (src 1 10) (Identifier "z")
      ]

  it "parses slice subscript" $
    parseExpr "a[b]"
    `shouldSucceedWith`
    Subscript (src 1 1)
      (Symbol (src 1 1) (Identifier "a"))
      [Singular (Symbol (src 1 3) (Identifier "b"))]

  it "parses sublices with closed beginning and end" $
    parseExpr "a[b:c]"
    `shouldSucceedWith`
    Subscript (src 1 1)
      (Symbol (src 1 1) (Identifier "a"))
      [Range (Symbol (src 1 3) (Identifier "b"))
              (Symbol (src 1 5) (Identifier "c"))]

  it "parses subslices with open start" $
    parseExpr "a[:c]"
    `shouldSucceedWith`
    Subscript (src 1 1)
      (Symbol (src 1 1) (Identifier "a"))
      [RangeTo (Symbol (src 1 4) (Identifier "c"))]

  it "parses subslices with open ending" $
    parseExpr "a[b:]"
    `shouldSucceedWith`
    Subscript (src 1 1)
      (Symbol (src 1 1) (Identifier "a"))
      [RangeFrom (Symbol (src 1 3) (Identifier "b"))]

  it "fails on subslices with open start and end" $
    shouldFail $ parseExpr "a[:]"

  it "parses record initializer" $
    parseExpr "{ size = 1 }"
    `shouldSucceedWith`
    RecordInitializer (src 1 1)
      [FieldInitializer (src 1 3) (Identifier "size") (Literal (src 1 10) (Int 1))]

  it "parses record initializer containing multiple fields" $
    parseExpr "{ size = 1, foo = \"foo\" }"
    `shouldSucceedWith`
    RecordInitializer (src 1 1)
      [FieldInitializer (src 1 3) (Identifier "size") (Literal (src 1 10) (Int 1))
      ,FieldInitializer (src 1 13) (Identifier "foo") (Literal (src 1 19) (String "foo"))]

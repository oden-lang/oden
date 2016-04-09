{-# LANGUAGE OverloadedStrings #-}
module Oden.Parser.ParseTopLevelSpec where

import           Test.Hspec

import           Oden.Identifier
import           Oden.Parser
import           Oden.SourceInfo
import           Oden.Syntax
import           Oden.Type.Signature

import           Oden.Assertions

src :: Line -> Column -> SourceInfo
src l c = SourceInfo (Position "<stdin>" l c)

spec :: Spec
spec = describe "parseTopLevel" $ do
  it "parses type signature" $
    parseTopLevel "x : int"
    `shouldSucceedWith`
    TypeSignatureDeclaration (src 1 1) (Identifier "x") (TypeSignature (src 1 5) [] (TSSymbol (src 1 5) (Identifier "int")))

  it "parses fn type signature" $
    parseTopLevel "x : int -> int"
    `shouldSucceedWith`
    TypeSignatureDeclaration
    (src 1 1)
    (Identifier "x")
    (TypeSignature (src 1 5) [] (TSFn (src 1 5) (TSSymbol (src 1 5) (Identifier "int")) (TSSymbol (src 1 12) (Identifier "int"))))

  it "parses type signature with no-arg fn" $
    parseTopLevel "x : -> ()"
    `shouldSucceedWith`
    TypeSignatureDeclaration
    (src 1 1)
    (Identifier "x")
    (TypeSignature (src 1 5) [] (TSNoArgFn (src 1 5) (TSUnit (src 1 8))))

  it "parses type signature with int slice" $
    parseTopLevel "x : []{int}"
    `shouldSucceedWith`
    TypeSignatureDeclaration
    (src 1 1)
    (Identifier "x")
    (TypeSignature (src 1 5) [] (TSSlice (src 1 5) (TSSymbol (src 1 8) (Identifier "int"))))

  it "parses type signature with string slice" $
    parseTopLevel "x : []{string}"
    `shouldSucceedWith`
    TypeSignatureDeclaration
    (src 1 1)
    (Identifier "x")
    (TypeSignature (src 1 5) [] (TSSlice (src 1 5) (TSSymbol (src 1 8) (Identifier "string"))))

  it "parses polymorphic quantified type signature" $
    parseTopLevel "x : forall a. a -> a"
    `shouldSucceedWith`
    TypeSignatureDeclaration
    (src 1 1)
    (Identifier "x")
    (TypeSignature
      (src 1 5)
      [SignatureVarBinding (src 1 12) (Identifier "a")]
      (TSFn (src 1 15) (TSSymbol (src 1 15) (Identifier "a")) (TSSymbol (src 1 20) (Identifier "a"))))

  it "parses type signature with row variable record" $
    parseTopLevel "foo : forall a r. { x: a, y: a | r }"
    `shouldSucceedWith`
    TypeSignatureDeclaration
    (src 1 1)
    (Identifier "foo")
    (TypeSignature
      (src 1 7)
      [SignatureVarBinding (src 1 14) (Identifier "a"),
      SignatureVarBinding (src 1 16) (Identifier "r")]
      (TSRecord
      (src 1 19)
      (TSRowExtension
        (src 1 21)
        (Identifier "x")
        (TSSymbol (src 1 24) (Identifier "a"))
        (TSRowExtension
          (src 1 27)
          (Identifier "y")
          (TSSymbol (src 1 30) (Identifier "a"))
          (TSSymbol (src 1 34) (Identifier "r"))))))

  it "parses record definition without type parameters" $
    parseTopLevel "type S = {\n  x: T\n}"
    `shouldSucceedWith`
    TypeDefinition
    (src 1 1)
    (Identifier "S")
    (TSRecord
      (src 1 10)
      (TSRowExtension
      (src 2 3)
      (Identifier "x")
      (TSSymbol (src 2 6) (Identifier "T"))
      (TSRowEmpty (src 1 10))))

  it "parses record definition with multiple fields" $
    parseTopLevel "type S = {\n  x: T,\n  y: T\n}"
    `shouldSucceedWith`
    TypeDefinition
    (src 1 1)
    (Identifier "S")
    (TSRecord
      (src 1 10)
      (TSRowExtension
      (src 2 3)
      (Identifier "x")
      (TSSymbol (src 2 6) (Identifier "T"))
      (TSRowExtension
        (src 3 3)
        (Identifier "y")
        (TSSymbol (src 3 6) (Identifier "T"))
        (TSRowEmpty (src 1 10)))))

  it "parses value definition" $
    parseTopLevel "x = y"
    `shouldSucceedWith`
    ValueDefinition (src 1 1) (Identifier "x") (Symbol (src 1 5) (Identifier "y"))

  it "parses fn definition" $
    parseTopLevel "f = (x) -> x"
    `shouldSucceedWith`
    ValueDefinition (src 1 1) (Identifier "f") (Fn (src 1 5) [NameBinding (src 1 6) (Identifier "x")] (Symbol (src 1 12) (Identifier "x")))

  it "parses short-hand fn definition" $
    parseTopLevel "f(x) = x"
    `shouldSucceedWith`
    FnDefinition (src 1 1) (Identifier "f") [NameBinding (src 1 3) (Identifier "x")] (Symbol (src 1 8) (Identifier "x"))

  it "parses short-hand no-arg fn definition" $
    parseTopLevel "sideEffect() = x"
    `shouldSucceedWith`
    FnDefinition (src 1 1) (Identifier "sideEffect") [] (Symbol (src 1 16) (Identifier "x"))

  it "parses short-hand multi-arg fn definition" $
    parseTopLevel "f(x, y, z) = x"
    `shouldSucceedWith`
    FnDefinition (src 1 1) (Identifier "f")
    [NameBinding (src 1 3) (Identifier "x"), NameBinding (src 1 6) (Identifier "y"), NameBinding (src 1 9) (Identifier "z")]
    (Symbol (src 1 14) (Identifier "x"))


{-# LANGUAGE OverloadedStrings #-}
module Oden.Parser.ParseImplementationSpec where

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
spec = do
  it "parses an empty implementation" $
    parseTopLevel "impl Empty(int) {}"
    `shouldSucceedWith`
    Implementation
    (src 1 1)
    (TypeSignature
     (src 1 6)
     []
     (TSApp
      (src 1 12)
      (TSSymbol (src 1 6) (Identifier "Empty"))
      (TSSymbol (src 1 12) (Identifier "int"))))
    []

  it "parses an implementation with one method" $
    parseTopLevel "impl Empty(int) {\n  foo(n) = n\n}"
    `shouldSucceedWith`
    Implementation
    (src 1 1)
    (TypeSignature
     (src 1 6)
     []
     (TSApp
      (src 1 12)
      (TSSymbol (src 1 6) (Identifier "Empty"))
      (TSSymbol (src 1 12) (Identifier "int"))))
    [FnDefinition
     (src 2 3)
     (Identifier "foo")
     [NameBinding (src 2 7) (Identifier "n")]
     (Symbol (src 2 12) (Identifier "n"))]

  it "parses an implementation with two methods" $
    parseTopLevel "impl Empty(int) {\n  foo(n) = n\n  bar(n) = n\n}"
    `shouldSucceedWith`
    Implementation
    (src 1 1)
    (TypeSignature
     (src 1 6)
     []
     (TSApp
      (src 1 12)
      (TSSymbol (src 1 6) (Identifier "Empty"))
      (TSSymbol (src 1 12) (Identifier "int"))))
    [ FnDefinition
      (src 2 3)
      (Identifier "foo")
      [NameBinding (src 2 7) (Identifier "n")]
      (Symbol (src 2 12) (Identifier "n"))
    , FnDefinition
      (src 3 3)
      (Identifier "bar")
      [NameBinding (src 3 7) (Identifier "n")]
      (Symbol (src 3 12) (Identifier "n"))
    ]

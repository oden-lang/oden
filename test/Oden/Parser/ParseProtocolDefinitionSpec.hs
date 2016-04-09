{-# LANGUAGE OverloadedStrings #-}
module Oden.Parser.ParseProtocolDefinitionSpec where

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
  it "parses an empty protocol definition" $
    parseTopLevel "protocol Empty(a) {}"
    `shouldSucceedWith`
    ProtocolDefinition
    (src 1 1)
    (Identifier "Empty")
    (SignatureVarBinding (src 1 16) (Identifier "a"))
    []

  it "parses a protocol definition with one method" $
    parseTopLevel "protocol Num(a) {\n  asInt : a -> int\n}"
    `shouldSucceedWith`
    ProtocolDefinition
    (src 1 1)
    (Identifier "Num")
    (SignatureVarBinding (src 1 14) (Identifier "a"))
    [ProtocolMethodSignature
     (src 2 3)
     (Identifier "asInt")
     (TypeSignature
      (src 2 11)
      []
      (TSFn
       (src 2 11)
       (TSSymbol (src 2 11) (Identifier "a"))
       (TSSymbol (src 2 16) (Identifier "int"))))]

  it "parses a protocol definition with one method" $
    parseTopLevel "protocol Num(a) {\n  asInt : a -> int\n  zero : a\n}"
    `shouldSucceedWith`
    ProtocolDefinition
    (src 1 1)
    (Identifier "Num")
    (SignatureVarBinding (src 1 14) (Identifier "a"))
    [ProtocolMethodSignature
     (src 2 3)
     (Identifier "asInt")
     (TypeSignature
      (src 2 11)
      []
      (TSFn
       (src 2 11)
       (TSSymbol (src 2 11) (Identifier "a"))
       (TSSymbol (src 2 16) (Identifier "int")))),
     ProtocolMethodSignature
     (src 3 3)
     (Identifier "zero")
     (TypeSignature
      (src 3 10)
      []
      (TSSymbol (src 3 10) (Identifier "a")))]

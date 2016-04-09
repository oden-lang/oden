{-# LANGUAGE OverloadedStrings #-}
module Oden.Parser.ParsePackageSpec where

import           Test.Hspec

import           Oden.Parser
import           Oden.SourceInfo
import           Oden.Syntax

import           Oden.Assertions

src :: Line -> Column -> SourceInfo
src l c = SourceInfo (Position "<stdin>" l c)

spec :: Spec
spec = describe "parsePackage" $ do
  it "parses package declaration" $
    parsePackage "<stdin>" "package foo/bar"
    `shouldSucceedWith`
    Package (PackageDeclaration (src 1 1) ["foo", "bar"]) []

  it "parses imports" $
    parsePackage "<stdin>" "package foo/bar\nimport bar/baz\nimport foo\nimport github.com/foo/bar"
    `shouldSucceedWith`
    Package (PackageDeclaration (src 1 1) ["foo", "bar"]) [
        ImportDeclaration (src 2 1) ["bar", "baz"],
        ImportDeclaration (src 3 1) ["foo"],
        ImportDeclaration (src 4 1) ["github.com", "foo", "bar"]
      ]

  it "parses github.com import" $
    parsePackage "<stdin>" "package foo/bar\nimport github.com/foo/bar"
    `shouldSucceedWith`
    Package (PackageDeclaration (src 1 1) ["foo", "bar"]) [ImportDeclaration (src 2 1) ["github.com", "foo", "bar"]]

  it "parses definitions" $
    parsePackage "<stdin>" "package foo/bar\nimport bar/baz"
    `shouldSucceedWith`
    Package (PackageDeclaration (src 1 1) ["foo", "bar"]) [ImportDeclaration (src 2 1) ["bar", "baz"]]

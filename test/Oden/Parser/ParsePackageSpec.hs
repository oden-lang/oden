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

  it "parses foreign imports" $
    parsePackage "<stdin>" "package foo/bar\nimport foreign \"bar/baz\"\nimport foreign \"foo\"\nimport foreign \"github.com/foo/bar\""
    `shouldSucceedWith`
    Package
    (PackageDeclaration (src 1 1) ["foo", "bar"])
    [ ImportForeignDeclaration (src 2 1) "bar/baz"
    , ImportForeignDeclaration (src 3 1) "foo"
    , ImportForeignDeclaration (src 4 1) "github.com/foo/bar"
    ]

  it "parses foreign github.com import" $
    parsePackage "<stdin>" "package foo/bar\nimport foreign \"github.com/foo/bar\""
    `shouldSucceedWith`
    Package
    (PackageDeclaration (src 1 1) ["foo", "bar"])
    [ImportForeignDeclaration (src 2 1) "github.com/foo/bar"]

  it "parses native imports" $
    parsePackage "<stdin>" "package foo/bar\nimport bar/baz\nimport foo\nimport company/foo/bar"
    `shouldSucceedWith`
    Package
    (PackageDeclaration (src 1 1) ["foo", "bar"])
    [ ImportDeclaration (src 2 1) ["bar", "baz"]
    , ImportDeclaration (src 3 1) ["foo"]
    , ImportDeclaration (src 4 1) ["company", "foo", "bar"]
    ]

  it "parses native and foreign and imports" $
    parsePackage "<stdin>" "package foo/bar\nimport foo_bar/baz\nimport foreign \"foo\""
    `shouldSucceedWith`
    Package
    (PackageDeclaration (src 1 1) ["foo", "bar"])
    [ ImportDeclaration (src 2 1) ["foo_bar", "baz"]
    , ImportForeignDeclaration (src 3 1) "foo"
    ]

  it "fails to parse URL-like import name" $
    shouldFail (parsePackage "<stdin>" "package foo\nimport github.com/mypkg")

  it "fails to parse import name with dash" $
    shouldFail (parsePackage "<stdin>" "package foo\nimport company/their-pkg")

module Oden.Compiler.TypeEncoderSpec where

import           Test.Hspec

import           Oden.Compiler.TypeEncoder
import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.Type.Monomorphic
import           Oden.Type.Basic

missing :: Metadata SourceInfo
missing = Metadata Missing

spec :: Spec
spec =
  describe "encodeTypeInstance" $ do
    it "encodes arrow" $
      encodeTypeInstance (Identifier "foo") (TFn missing (TBasic missing TInt) (TBasic missing TString)) `shouldBe` "foo_inst_int_to_string"
    it "encodes nested arrows" $
      encodeTypeInstance (Identifier "foo") (TFn missing (TBasic missing TBool) (TFn missing (TBasic missing TInt) (TBasic missing TString))) `shouldBe` "foo_inst_bool_to_int__to__string"
    it "encodes single arrow" $
      encodeTypeInstance (Identifier "foo") (TNoArgFn missing (TBasic missing TInt)) `shouldBe` "foo_inst_to_int"
    it "encodes nested single arrows" $
      encodeTypeInstance (Identifier "foo") (TNoArgFn missing (TNoArgFn missing (TBasic missing TInt))) `shouldBe` "foo_inst_to_to__int"
    it "encodes uncurried func" $
      encodeTypeInstance (Identifier "foo") (TUncurriedFn missing [TBasic missing TBool, TBasic missing TInt] (TBasic missing TString)) `shouldBe` "foo_inst_bool_to_int_to_string"
    it "encodes variadic func" $
      encodeTypeInstance (Identifier "foo") (TVariadicFn missing [TBasic missing TBool] (TBasic missing TInt) (TBasic missing TString)) `shouldBe` "foo_inst_bool_to_variadic_int_to_string"
    it "encodes slice" $
      encodeTypeInstance (Identifier "foo") (TFn missing (TSlice missing (TBasic missing TBool)) (TSlice missing (TBasic missing TInt))) `shouldBe` "foo_inst_sliceof__bool_to_sliceof__int"

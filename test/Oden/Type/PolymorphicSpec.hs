module Oden.Type.PolymorphicSpec where

import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Test.Hspec

named :: String -> Type -> Type
named = TNamed Missing . FQN []

spec =
  describe "underlying" $ do
    it "returns unnamed types as-is" $
      underlying (TAny Missing) `shouldBe` (TAny Missing)

    it "returns the underlying named type one level down" $
      underlying (named "A" $ TAny Missing) `shouldBe` (TAny Missing)

    it "returns the underlying named type one level down" $
      underlying (named "A" $ named "B" $ TAny Missing) `shouldBe` (TAny Missing)

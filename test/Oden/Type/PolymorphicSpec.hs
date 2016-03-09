module Oden.Type.PolymorphicSpec where

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Test.Hspec

missing :: Metadata SourceInfo
missing = Metadata Missing

named :: String -> Type -> Type
named = TNamed missing . FQN [] . Identifier

spec =
  describe "underlying" $ do
    it "returns unnamed types as-is" $
      underlying (TAny missing) `shouldBe` (TAny missing)

    it "returns the underlying named type one level down" $
      underlying (named "A" $ TAny missing) `shouldBe` (TAny missing)

    it "returns the underlying named type one level down" $
      underlying (named "A" $ named "B" $ TAny missing) `shouldBe` (TAny missing)

module Oden.Infer.ConstraintCollectionSpec where

import           Oden.Core.Expr
import           Oden.Identifier
import           Oden.Infer.ConstraintCollection
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import qualified Data.Set as Set

import           Test.Hspec

missing :: Metadata SourceInfo
missing = Metadata Missing

tvA = TV "a"
tvB = TV "b"

typeInt :: Type
typeInt = TCon missing (nameInUniverse "int")

tvarA = TVar missing tvA
tvarB = TVar missing tvB

fooId = Identifier "foo"

protocolFooName = nameInUniverse "Foo"
protocolBarName = nameInUniverse "Bar"

protocolFoo = Protocol missing protocolFooName (TVar missing tvA) []
protocolBar = Protocol missing protocolBarName (TVar missing tvA) []

spec = describe "collectConstraints" $ do
  it "strips empty TConstrained" $
    let symbol = Symbol missing fooId (TConstrained Set.empty typeInt)
        unconstrained = Symbol missing fooId typeInt
    in collectConstraints symbol
    `shouldBe`
    (unconstrained, Set.empty)

  it "collects constraints" $
    let constraint = ProtocolConstraint missing protocolFooName tvarA
        symbol = Symbol missing fooId (TConstrained (Set.singleton constraint) tvarA)
        unconstrained = Symbol missing fooId tvarA
    in collectConstraints symbol
    `shouldBe`
    (unconstrained, Set.singleton constraint)

  it "collects nested constraints" $
    let fooConstraint = ProtocolConstraint missing protocolFooName tvarA
        barConstraint = ProtocolConstraint missing protocolBarName tvarA
        symbol = Symbol
                 missing
                 fooId
                 (TConstrained (Set.singleton barConstraint) (TConstrained (Set.singleton fooConstraint) tvarA))
        unconstrained = Symbol missing fooId tvarA
    in collectConstraints symbol
    `shouldBe`
    (unconstrained, Set.fromList [fooConstraint, barConstraint])

  it "collects unrelated nested constraints" $
    let fooConstraint = ProtocolConstraint missing protocolFooName tvarA
        barConstraint = ProtocolConstraint missing protocolBarName tvarB
        symbol = Symbol
                 missing
                 fooId
                 (TConstrained (Set.singleton barConstraint) (TConstrained (Set.singleton fooConstraint) tvarA))
        unconstrained = Symbol missing fooId tvarA
    in collectConstraints symbol
    `shouldBe`
    (unconstrained, Set.fromList [fooConstraint, barConstraint])

  it "collects constraints in different branches" $
    let fooConstraint = ProtocolConstraint missing protocolFooName tvarA
        barConstraint = ProtocolConstraint missing protocolBarName tvarB
        symbol = Symbol
                 missing
                 fooId
                 (TFn
                  missing
                  (TConstrained (Set.singleton fooConstraint) tvarA)
                  (TConstrained (Set.singleton barConstraint) tvarB))
        unconstrained = Symbol missing fooId (TFn missing tvarA tvarB)
    in collectConstraints symbol
    `shouldBe`
    (unconstrained, Set.fromList [fooConstraint, barConstraint])

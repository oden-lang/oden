module Oden.ExplodeSpec where

import qualified Oden.Core.Expr        as Expr
import qualified Oden.Core.Untyped     as Untyped
import           Oden.Core.Untyped     (Untyped(..))
import qualified Oden.Explode          as E
import           Oden.Explode          hiding (explodeTopLevel)
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Syntax
import           Oden.Type.Signature
import           Test.Hspec

import           Oden.Assertions

src :: Line -> Column -> SourceInfo
src l c = SourceInfo (Position "<test>" l c)

ignored :: Metadata SourceInfo
ignored = Metadata Missing

explodeTopLevel top = snd <$> E.explodeTopLevel ["pkg"] top

spec :: Spec
spec = do
  describe "explodeExpr" $ do
    it "converts symbol" $
       explodeExpr (Symbol (src 1 1) (Identifier "x"))
      `shouldSucceedWith`
      Expr.Symbol ignored (Identifier "x") Untyped

    it "converts int literal" $
       explodeExpr (Literal (src 1 1) (Int 1))
      `shouldSucceedWith`
      Expr.Literal ignored (Expr.Int 1) Untyped

    it "converts bool literal" $
       explodeExpr (Literal (src 1 1) (Bool True))
      `shouldSucceedWith`
      Expr.Literal ignored (Expr.Bool True) Untyped

    it "converts fn application with no params" $
       explodeExpr (Application
                   (src 1 1)
                   (Fn (src 1 1) [] (Symbol (src 1 3) (Identifier "x")))
                   [])
      `shouldSucceedWith`
      Expr.NoArgApplication
      ignored
      (Expr.NoArgFn
       ignored
       (Expr.Symbol ignored (Identifier "x") Untyped)
       Untyped)
      Untyped

    it "converts fn application with multiple params" $
       explodeExpr (Application
                   (src 1 1)
                   (Fn
                    (src 1 1)
                    [NameBinding (src 1 2) (Identifier "x"),
                     NameBinding (src 1 3) (Identifier "y")]
                    (Symbol (src 1 4) (Identifier "x")))
                   [Symbol (src 1 5) (Identifier "x"),
                    Symbol (src 1 9) (Identifier "y")])
      `shouldSucceedWith`
      Expr.Application
      ignored
      (Expr.Application
       ignored
       (Expr.Fn
        ignored
        (Expr.NameBinding ignored (Identifier "x"))
        (Expr.Fn
         ignored
         (Expr.NameBinding ignored (Identifier "y"))
         (Expr.Symbol ignored (Identifier "x") Untyped)
         Untyped)
        Untyped)
       (Expr.Symbol ignored (Identifier "x") Untyped)
       Untyped)
      (Expr.Symbol ignored (Identifier "y") Untyped)
      Untyped

  describe "explodeTopLevel" $ do
    it "converts fn definition with no argument" $
      explodeTopLevel
      [TopLevelDefinition
       (FnDefinition (src 1 1) (Identifier "f") [] (Symbol (src 1 3) (Identifier "x")))]
      `shouldSucceedWith`
      [Untyped.Definition
       ignored
       (Identifier "f")
       Nothing
       (Expr.NoArgFn
        ignored
        (Expr.Symbol ignored (Identifier "x") Untyped)
        Untyped)]

    it "converts fn definition with single argument" $
      explodeTopLevel
      [TopLevelDefinition
       (FnDefinition
        (src 1 1)
        (Identifier "f")
        [NameBinding (src 1 2) (Identifier "x")]
        (Symbol (src 1 3) (Identifier "x")))]
      `shouldSucceedWith`
      [Untyped.Definition
       ignored
       (Identifier "f")
       Nothing
       (Expr.Fn
        ignored
        (Expr.NameBinding ignored (Identifier "x"))
        (Expr.Symbol ignored (Identifier "x") Untyped)
        Untyped)]

    it "converts fn definition with multiple arguments" $
      explodeTopLevel
      [TopLevelDefinition
       (FnDefinition
        (src 1 1)
        (Identifier "f")
        [NameBinding (src 1 2) (Identifier "x"), NameBinding (src 1 3) (Identifier "y")]
        (Symbol (src 1 4) (Identifier "x")))]
      `shouldSucceedWith`
      [Untyped.Definition
       ignored
       (Identifier "f")
       Nothing
       (Expr.Fn
        ignored
        (Expr.NameBinding ignored (Identifier "x"))
        (Expr.Fn
         ignored
         (Expr.NameBinding ignored (Identifier "y"))
         (Expr.Symbol ignored (Identifier "x") Untyped)
         Untyped)
        Untyped)]

    it "converts struct definition and uses empty list for type parameters" $
      explodeTopLevel
      [TypeDefinition
       (src 1 1)
       (Identifier "S")
       (TSRecord (src 1 2) (TSRowExtension (src 1 3) (Identifier "x") (TSSymbol (src 1 4) (Identifier "t")) (TSRowEmpty (src 1 2))))]
      `shouldSucceedWith`
      [Untyped.TypeDefinition
       ignored
       (FQN ["pkg"] (Identifier "S"))
       []
       (TSRecord (src 1 2) (TSRowExtension (src 1 3) (Identifier "x") (TSSymbol (src 1 4) (Identifier "t")) (TSRowEmpty (src 1 2))))]

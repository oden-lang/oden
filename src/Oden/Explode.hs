module Oden.Explode
(
 ExplodeError(..),
 explodeExpr,
 explodeTopLevel,
 explodePackage
) where

import qualified Oden.Core.Untyped     as Untyped
import           Oden.Identifier
import           Oden.QualifiedName    (QualifiedName(..))
import           Oden.Syntax
import           Oden.SourceInfo
import           Oden.Type.Signature

import qualified Data.Map              as Map

data ExplodeError = TypeSignatureWithoutDefinition SourceInfo Name TypeSignature
                  deriving (Show, Eq)

explodeNameBinding :: NameBinding -> Untyped.NameBinding
explodeNameBinding (NameBinding si name) = Untyped.NameBinding si name

explodeExpr :: Expr -> Untyped.Expr
explodeExpr (Subscript si es [Singular e]) =
  Untyped.Subscript si (explodeExpr es) (explodeExpr e)
explodeExpr (Subscript si es [Range e1 e2]) =
  Untyped.Subslice si (explodeExpr es) (Untyped.Range (explodeExpr e1) (explodeExpr e2))
explodeExpr (Subscript si es [RangeTo e]) =
  Untyped.Subslice si (explodeExpr es) (Untyped.RangeTo (explodeExpr e))
explodeExpr (Subscript si es [RangeFrom e]) =
  Untyped.Subslice si (explodeExpr es) (Untyped.RangeFrom (explodeExpr e))
explodeExpr (Subscript si es (i:ir)) =
  explodeExpr (Subscript si (Subscript si es [i]) ir)

explodeExpr (UnaryOp si o e) =
  Untyped.UnaryOp si o (explodeExpr e)
explodeExpr (BinaryOp si o e1 e2) =
  Untyped.BinaryOp si o (explodeExpr e1) (explodeExpr e2)
explodeExpr (Symbol si i) =
  Untyped.Symbol si i
explodeExpr (Literal si (Bool b)) =
  Untyped.Literal si (Untyped.Bool b)
explodeExpr (Literal si (Int i)) =
  Untyped.Literal si (Untyped.Int i)
explodeExpr (Literal si (String s)) =
  Untyped.Literal si (Untyped.String s)
explodeExpr (Literal si Unit) =
  Untyped.Literal si Untyped.Unit
explodeExpr (Tuple si f s r) =
  Untyped.Tuple si (explodeExpr f) (explodeExpr s) (map explodeExpr r)
explodeExpr (If si c t f) =
  Untyped.If si (explodeExpr c) (explodeExpr t) (explodeExpr f)
explodeExpr (Application si f ps) =
  Untyped.Application si (explodeExpr f) (map explodeExpr ps)
explodeExpr (Fn si [] b) =
  Untyped.NoArgFn si (explodeExpr b)
explodeExpr (Fn si [arg] b) =
  Untyped.Fn si (explodeNameBinding arg) (explodeExpr b)
explodeExpr (Fn si (arg:args) b) =
  Untyped.Fn si (explodeNameBinding arg) (explodeExpr (Fn si args b))

-- invalid, but can be handled anyway
explodeExpr (Subscript _ a []) = explodeExpr a
explodeExpr (Let _ [] b) = explodeExpr b
explodeExpr (Let _ [LetPair si n e] b) =
  Untyped.Let si (explodeNameBinding n) (explodeExpr e) (explodeExpr b)
explodeExpr (Let si (LetPair _ n e:bs) b) =
  Untyped.Let si (explodeNameBinding n) (explodeExpr e) (explodeExpr (Let si bs b))
explodeExpr (Slice si es) =
  Untyped.Slice si (map explodeExpr es)
explodeExpr (Block si es) =
  Untyped.Block si (map explodeExpr es)

explodeStructField :: StructFieldExpr -> Untyped.StructField
explodeStructField (StructFieldExpr si name t) =
  Untyped.StructField si name t

explodeTopLevel :: PackageName -> [TopLevel] -> Either [ExplodeError] ([Untyped.Import], [Untyped.Definition])
explodeTopLevel pkg top =
  let (is, scs, defs) = foldl iter ([], Map.empty, []) top
  in case Map.assocs scs of
    [] -> Right (is, defs)
    as -> Left (map toSignatureError as)
  where iter (is, ts, defs) (FnDefinition si name args body) =
          let def = Untyped.Definition si name (snd <$> Map.lookup name ts) (explodeExpr (Fn si args body))
          in (is, Map.delete name ts, defs ++ [def])
        iter (is, ts, defs) (ValueDefinition si name expr) =
          let def = Untyped.Definition si name (snd <$> Map.lookup name ts) (explodeExpr expr)
          in (is, Map.delete name ts, defs ++ [def])
        iter (is, ts, defs) (TypeSignatureDeclaration tsi name sc) =
          (is, Map.insert name (tsi, sc) ts, defs)
        iter (is, ts, defs) (ImportDeclaration si name) =
          (is ++ [Untyped.Import si name], ts, defs)
        iter (is, ts, defs) (StructDefinition si name params fields) =
          let def = Untyped.StructDefinition si (FQN pkg name) (map explodeNameBinding params) (map explodeStructField fields)
          in (is, Map.delete name ts, defs ++ [def])
        toSignatureError :: (Name, (SourceInfo, TypeSignature)) -> ExplodeError
        toSignatureError (n, (si, sc)) = TypeSignatureWithoutDefinition si n sc

explodePackage :: Package -> Either [ExplodeError] Untyped.Package
explodePackage (Package (PackageDeclaration si name) definitions) = do
  (is, ds) <- explodeTopLevel name definitions
  return (Untyped.Package (Untyped.PackageDeclaration si name) is ds)

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

import           Control.Monad
import qualified Data.Map              as Map

data ExplodeError = TypeSignatureWithoutDefinition SourceInfo Identifier TypeSignature
                  | InvalidMemberAccessExpression SourceInfo Untyped.Expr Untyped.Expr
                  deriving (Show, Eq)

explodeNameBinding :: NameBinding -> Untyped.NameBinding
explodeNameBinding (NameBinding si name) = Untyped.NameBinding si name

explodeExpr :: Expr -> Either ExplodeError Untyped.Expr
explodeExpr (Subscript si es [Singular e]) =
  Untyped.Subscript si <$> explodeExpr es <*> explodeExpr e
explodeExpr (Subscript si es [Range e1 e2]) =
  Untyped.Subslice si <$> explodeExpr es <*> (Untyped.Range <$> explodeExpr e1 <*> explodeExpr e2)
explodeExpr (Subscript si es [RangeTo e]) =
  Untyped.Subslice si <$> explodeExpr es <*> (Untyped.RangeTo <$> explodeExpr e)
explodeExpr (Subscript si es [RangeFrom e]) =
  Untyped.Subslice si <$> explodeExpr es <*> (Untyped.RangeFrom <$> explodeExpr e)
explodeExpr (Subscript si es (i:ir)) =
  explodeExpr (Subscript si (Subscript si es [i]) ir)

explodeExpr (UnaryOp si o e) =
  Untyped.UnaryOp si o <$> explodeExpr e
explodeExpr (BinaryOp si o e1 e2) =
  Untyped.BinaryOp si o <$> explodeExpr e1 <*> explodeExpr e2
explodeExpr (Symbol si i) =
  return $ Untyped.Symbol si i
explodeExpr (Literal si (Bool b)) =
  return $ Untyped.Literal si (Untyped.Bool b)
explodeExpr (Literal si (Int i)) =
  return $ Untyped.Literal si (Untyped.Int i)
explodeExpr (Literal si (String s)) =
  return $ Untyped.Literal si (Untyped.String s)
explodeExpr (Literal si Unit) =
  return $ Untyped.Literal si Untyped.Unit
explodeExpr (Tuple si f s r) =
  Untyped.Tuple si <$> explodeExpr f <*> explodeExpr s <*> mapM explodeExpr r
explodeExpr (If si c t f) =
  Untyped.If si <$> explodeExpr c <*> explodeExpr t <*> explodeExpr f
explodeExpr (Application si f ps) =
  Untyped.Application si <$> explodeExpr f <*> mapM explodeExpr ps
explodeExpr (Fn si [] b) =
  Untyped.NoArgFn si <$> explodeExpr b
explodeExpr (Fn si [arg] b) =
  Untyped.Fn si (explodeNameBinding arg) <$> explodeExpr b
explodeExpr (Fn si (arg:args) b) =
  Untyped.Fn si (explodeNameBinding arg) <$> explodeExpr (Fn si args b)
explodeExpr (StructInitializer si ts exprs) =
  Untyped.StructInitializer si ts <$> mapM explodeExpr exprs
explodeExpr (MemberAccess si expr (Symbol _ name)) =
  Untyped.MemberAccess si <$> explodeExpr expr <*> return name
explodeExpr (MemberAccess si expr nonName) = do
  expr' <- explodeExpr expr
  nonName' <- explodeExpr nonName
  Left $ InvalidMemberAccessExpression si expr' nonName'

-- invalid, but can be handled anyway
explodeExpr (Subscript _ a []) = explodeExpr a
explodeExpr (Let _ [] b) = explodeExpr b
explodeExpr (Let _ [LetPair si n e] b) =
  Untyped.Let si (explodeNameBinding n) <$> explodeExpr e <*> explodeExpr b
explodeExpr (Let si (LetPair _ n e:bs) b) =
  Untyped.Let si (explodeNameBinding n) <$> explodeExpr e <*> explodeExpr (Let si bs b)
explodeExpr (Slice si es) =
  Untyped.Slice si <$> mapM explodeExpr es
explodeExpr (Block si es) =
  Untyped.Block si <$> mapM explodeExpr es

explodeTopLevel :: PackageName -> [TopLevel] -> Either [ExplodeError] ([Untyped.ImportReference], [Untyped.Definition])
explodeTopLevel pkg top =
  case foldM iter ([], Map.empty, []) top of
    Right (is, scs, defs) ->
      case Map.assocs scs of
        [] -> return (is, defs)
        as -> Left (map toSignatureError as)
    -- TODO: Catch error in multiple top level expressions
    Left e -> Left [e]
  where
  iter (is, ts, defs) (FnDefinition si name args body) = do
    def <- Untyped.Definition si name (snd <$> Map.lookup name ts) <$> explodeExpr (Fn si args body)
    return (is, Map.delete name ts, defs ++ [def])
  iter (is, ts, defs) (ValueDefinition si name expr) = do
    def <- Untyped.Definition si name (snd <$> Map.lookup name ts) <$> explodeExpr expr
    return (is, Map.delete name ts, defs ++ [def])
  iter (is, ts, defs) (TypeSignatureDeclaration tsi name sc) =
    return (is, Map.insert name (tsi, sc) ts, defs)
  iter (is, ts, defs) (ImportDeclaration si name) =
    return (is ++ [Untyped.ImportReference si name], ts, defs)
  iter (is, ts, defs) (TypeDefinition si name typeSig) =
    -- TODO: Add support for type parameters
    let def = Untyped.TypeDefinition si (FQN pkg name) [] typeSig
    in return (is, Map.delete name ts, defs ++ [def])
  toSignatureError :: (Identifier, (SourceInfo, TypeSignature)) -> ExplodeError
  toSignatureError (n, (si, sc)) = TypeSignatureWithoutDefinition si n sc

explodePackage :: Package -> Either [ExplodeError] (Untyped.Package [Untyped.ImportReference])
explodePackage (Package (PackageDeclaration si name) definitions) = do
  (is, ds) <- explodeTopLevel name definitions
  return (Untyped.Package (Untyped.PackageDeclaration si name) is ds)

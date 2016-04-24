{-# LANGUAGE LambdaCase #-}
module Oden.Explode
(
 ExplodeError(..),
 explodeExpr,
 explodeTopLevel,
 explodePackage
) where

import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.Untyped
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName    (QualifiedName(..))
import qualified Oden.Syntax as Syntax
import           Oden.SourceInfo
import           Oden.Type.Signature

import           Control.Monad
import           Control.Monad.Writer
import qualified Data.Map              as Map

data ExplodeError = TypeSignatureWithoutDefinition SourceInfo Identifier (TypeSignature SourceInfo)
                  | TypeSignatureRedefinition SourceInfo Identifier (Maybe (TypeSignature SourceInfo))
                  | InvalidMemberAccessExpression SourceInfo UntypedExpr UntypedExpr
                  | InvalidProtocolMethodReference SourceInfo UntypedExpr UntypedExpr
                  deriving (Show, Eq)

explodeNameBinding :: Syntax.NameBinding -> NameBinding
explodeNameBinding (Syntax.NameBinding si name) = NameBinding (Metadata si) name

explodeFieldInitializer :: Syntax.FieldInitializer
                        -> Either ExplodeError (FieldInitializer UntypedExpr)
explodeFieldInitializer (Syntax.FieldInitializer si label expr) =
  FieldInitializer (Metadata si) label <$> explodeExpr expr

untyped :: Either ExplodeError Untyped
untyped = pure Untyped

explodeExpr :: Syntax.Expr -> Either ExplodeError UntypedExpr
explodeExpr = \case
  Syntax.Subscript si es [Syntax.Singular e] ->
    Subscript (Metadata si) <$> explodeExpr es <*> explodeExpr e <*> untyped
  Syntax.Subscript si es [Syntax.Range e1 e2] ->
    Subslice (Metadata si) <$> explodeExpr es <*> (Range <$> explodeExpr e1 <*> explodeExpr e2) <*> untyped
  Syntax.Subscript si es [Syntax.RangeTo e] ->
    Subslice (Metadata si) <$> explodeExpr es <*> (RangeTo <$> explodeExpr e) <*> untyped
  Syntax.Subscript si es [Syntax.RangeFrom e] ->
    Subslice (Metadata si) <$> explodeExpr es <*> (RangeFrom <$> explodeExpr e) <*> untyped
  Syntax.Subscript si es (i:ir) ->
    explodeExpr (Syntax.Subscript si (Syntax.Subscript si es [i]) ir)

  Syntax.UnaryOp si o e ->
    UnaryOp (Metadata si) o <$> explodeExpr e <*> untyped
  Syntax.BinaryOp si o e1 e2 ->
    BinaryOp (Metadata si) o <$> explodeExpr e1 <*> explodeExpr e2 <*> untyped
  Syntax.Symbol si i ->
    return $ Symbol (Metadata si) i Untyped
  Syntax.Literal si (Syntax.Bool b) ->
    return $ Literal (Metadata si) (Bool b) Untyped
  Syntax.Literal si (Syntax.Int i) ->
    return $ Literal (Metadata si) (Int i) Untyped
  Syntax.Literal si (Syntax.String s) ->
    return $ Literal (Metadata si) (String s) Untyped
  Syntax.Literal si Syntax.Unit ->
    return $ Literal (Metadata si) Unit Untyped
  Syntax.Tuple si f s r ->
    Tuple (Metadata si) <$> explodeExpr f <*> explodeExpr s <*> mapM explodeExpr r <*> untyped
  Syntax.If si c t f ->
    If (Metadata si) <$> explodeExpr c <*> explodeExpr t <*> explodeExpr f <*> untyped
  Syntax.Application si f [] ->
    NoArgApplication (Metadata si) <$> explodeExpr f <*> untyped
  Syntax.Application si f [p] ->
    Application (Metadata si) <$> explodeExpr f <*> explodeExpr p <*> untyped
  Syntax.Application si f ps ->
    Application (Metadata si) <$> explodeExpr (Syntax.Application si f (init ps))
                              <*> explodeExpr (last ps)
                              <*> untyped
  Syntax.Fn si [] b ->
    NoArgFn (Metadata si) <$> explodeExpr b <*> untyped
  Syntax.Fn si [arg] b ->
    Fn (Metadata si) (explodeNameBinding arg) <$> explodeExpr b <*> untyped
  Syntax.Fn si (arg:args) b ->
    Fn (Metadata si) (explodeNameBinding arg) <$> explodeExpr (Syntax.Fn si args b) <*> untyped
  Syntax.RecordInitializer si fields ->
    RecordInitializer (Metadata si) <$> mapM explodeFieldInitializer fields <*> return Untyped
  Syntax.MemberAccess si expr (Syntax.Symbol _ name) ->
    let memberAccess = NamedMemberAccess <$> explodeExpr expr <*> return name
    in MemberAccess (Metadata si) <$> memberAccess <*> untyped
  Syntax.MemberAccess si expr nonName -> do
    expr' <- explodeExpr expr
    nonName' <- explodeExpr nonName
    Left (InvalidMemberAccessExpression si expr' nonName')
  -- invalid, but can be handled anyway
  Syntax.Subscript _ a [] -> explodeExpr a
  Syntax.Let _ [] b -> explodeExpr b
  Syntax.Let _ [Syntax.LetPair si n e] b ->
    Let (Metadata si) (explodeNameBinding n) <$> explodeExpr e <*> explodeExpr b <*> untyped
  Syntax.Let si (Syntax.LetPair _ n e:bs) b ->
    Let (Metadata si) (explodeNameBinding n) <$> explodeExpr e <*> explodeExpr (Syntax.Let si bs b) <*> untyped
  Syntax.Slice si es ->
    Slice (Metadata si) <$> mapM explodeExpr es <*> untyped
  Syntax.Block si es ->
    Block (Metadata si) <$> mapM explodeExpr es <*> untyped
  Syntax.ProtocolMethodReference si (Syntax.Symbol _ protocol) (Syntax.Symbol _ method) ->
    return (MethodReference (Metadata si) (NamedMethodReference protocol method) Untyped)
  Syntax.ProtocolMethodReference si lhs rhs -> do
    lhs' <- explodeExpr lhs
    rhs' <- explodeExpr rhs
    Left (InvalidMemberAccessExpression si lhs' rhs')

-- temporary metadata for top level definitions, used for keeping track
-- of duplications and detecting missing terms for type signatures
data TempTopLevel = TempTop {
    tempType :: (SourceInfo, Maybe (TypeSignature SourceInfo)),
    -- whether this signature has a corresponding definition
    hasValue :: Bool
}

collectError :: Either e a -> Writer [e] (Maybe a)
collectError e = case e of
  Left err -> do
    tell [err]
    return Nothing
  Right x -> return (Just x)

explodeTopLevel' :: PackageName
                 -> [Syntax.TopLevel]
                 -> Writer [ExplodeError] ([ImportReference], [Definition])
explodeTopLevel' pkg top = do
  (is, scs, defs) <- foldM iter ([], Map.empty, []) top
  case filter (not . hasValue . snd) (Map.assocs scs) of
    [] -> return ()
    us -> mapM_ (tell . (:[]) . typeSigNoValErr) us
  return (is, defs)
  where
  iter :: ([ImportReference], Map.Map Identifier TempTopLevel, [Definition])
       -> Syntax.TopLevel
       -> Writer [ExplodeError] ([ImportReference], Map.Map Identifier TempTopLevel, [Definition])
  iter (is, ts, defs) (Syntax.FnDefinition si name args body) = do
    expr' <- collectError (explodeExpr (Syntax.Fn si args body))
    case expr' of
      Nothing -> return (is, ts, defs)
      Just expr ->
        let def = Definition (Metadata si) name (Map.lookup name ts >>= snd . tempType) expr
        in return (is, assignValue name si ts, defs ++ [def])
  iter (is, ts, defs) (Syntax.ValueDefinition si name expr) = do
    expr' <- collectError (explodeExpr expr)
    case expr' of
      Nothing -> return (is, ts, defs)
      Just e ->
        let def = Definition (Metadata si) name (Map.lookup name ts >>= snd . tempType) e
        in return (is, assignValue name si ts, defs ++ [def])
  iter (is, ts, defs) (Syntax.TypeSignatureDeclaration tsi name signature) = do
    case Map.lookup name ts of
      Just existing -> tell [typeSigReDefErr (name, existing) tsi] -- type already defined
      Nothing -> return ()
    return (is, newTypeSig name tsi (Just signature) ts, defs)
  iter (is, ts, defs) (Syntax.ImportDeclaration si name) =
    return (is ++ [ImportReference (Metadata si) name], ts, defs)
  iter (is, ts, defs) (Syntax.TypeDefinition si name typeSig) =
    -- TODO: Add support for type parameters
    let def = TypeDefinition (Metadata si) (FQN pkg name) [] typeSig
    in return (is, ts, defs ++ [def])
  iter (is, ts, defs) (Syntax.ProtocolDefinition si name varBinding methods) =
    let def = ProtocolDefinition (Metadata si) (FQN pkg name) varBinding methods
    in return (is, ts, defs ++ [def])
  newTypeSig name tsi msc =
    Map.insertWith (\_ old -> old) name (TempTop (tsi, msc) False)
  assignValue name si =
    -- if there's type signature, keep its location
    Map.insertWith (\_ (TempTop (_, sc') _) -> (TempTop (si, sc') True))
                     name (TempTop (si, Nothing) True)
  -- type signature doesn't have an assigned term
  typeSigNoValErr :: (Identifier, TempTopLevel) -> ExplodeError
  typeSigNoValErr (n, TempTop (si, sc) _)
      = case sc of
          Just j_sc -> TypeSignatureWithoutDefinition si n j_sc
          -- if this happens, it's a bug in the compiler, rather than source code
          Nothing  -> error "Panic: type signature definition present without actual signature"
  -- type signature already defined
  typeSigReDefErr :: (Identifier, TempTopLevel) -> SourceInfo -> ExplodeError
  typeSigReDefErr (n, TempTop (_, sc) _) si' = TypeSignatureRedefinition si' n sc

toEither :: (Eq v, Show v, Show e) => Writer [e] v -> Either [e] v
toEither w =
  case runWriter w of
    (a, []) -> Right a
    (_, es) -> Left es

explodeTopLevel :: PackageName -> [Syntax.TopLevel] -> Either [ExplodeError] ([ImportReference], [Definition])
explodeTopLevel = (.) toEither . explodeTopLevel'

explodePackage' :: Syntax.Package -> Writer [ExplodeError] (UntypedPackage ImportReference)
explodePackage' (Syntax.Package (Syntax.PackageDeclaration si name) definitions) = do
  (is, ds) <- explodeTopLevel' name definitions
  return (Package (PackageDeclaration (Metadata si) name) is ds)

explodePackage :: Syntax.Package -> Either [ExplodeError] (UntypedPackage ImportReference)
explodePackage p = case runWriter (explodePackage' p) of
  (a, []) -> Right a
  (_, errs) -> Left errs

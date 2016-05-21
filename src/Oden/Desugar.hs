{-# LANGUAGE LambdaCase #-}
module Oden.Desugar
(
 DesugarError(..),
 desugarExpr,
 desugarTopLevel,
 desugarPackage
) where

import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.Untyped

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName    (QualifiedName(..))
import qualified Oden.Syntax as Syntax
import           Oden.Syntax (UnaryOperator(..), BinaryOperator(..))
import           Oden.SourceInfo
import           Oden.Type.Signature

import           Control.Monad
import           Control.Monad.Writer
import qualified Data.Map              as Map

data DesugarError = TypeSignatureWithoutDefinition SourceInfo Identifier (TypeSignature SourceInfo)
                  | TypeSignatureRedefinition SourceInfo Identifier (Maybe (TypeSignature SourceInfo))
                  | InvalidMemberAccessExpression SourceInfo UntypedExpr UntypedExpr
                  | InvalidProtocolMethodReference SourceInfo UntypedExpr UntypedExpr
                  deriving (Show, Eq)

desugarNameBinding :: Syntax.NameBinding -> NameBinding
desugarNameBinding (Syntax.NameBinding si name) = NameBinding (Metadata si) name

desugarFieldInitializer :: Syntax.FieldInitializer
                        -> Either DesugarError (FieldInitializer UntypedExpr)
desugarFieldInitializer (Syntax.FieldInitializer si label expr) =
  FieldInitializer (Metadata si) label <$> desugarExpr expr

methodForUnaryOperator :: Syntax.UnaryOperator -> NamedMethodReference
methodForUnaryOperator =
  \case
    Syntax.Negate -> NamedMethodReference (Identifier "Num") (Identifier "Negate")
    Syntax.Not    -> NamedMethodReference (Identifier "Logical") (Identifier "Not")

methodForBinaryOperator :: BinaryOperator -> NamedMethodReference
methodForBinaryOperator =
  \case
    Add              -> NamedMethodReference (Identifier "Addition") (Identifier "Add")
    Subtract         -> NamedMethodReference (Identifier "Subtraction") (Identifier "Subtract")
    Multiply         -> NamedMethodReference (Identifier "Multiplication") (Identifier "Multiply")
    Divide           -> NamedMethodReference (Identifier "Division") (Identifier "Divide")
    EqualTo          -> NamedMethodReference (Identifier "Equality") (Identifier "EqualTo")
    NotEqualTo       -> NamedMethodReference (Identifier "Equality") (Identifier "NotEqualTo")
    MonoidApply      -> NamedMethodReference (Identifier "Monoid") (Identifier "Apply")
    LessThan         -> NamedMethodReference (Identifier "Ordered") (Identifier "LessThan")
    LessThanEqual    -> NamedMethodReference (Identifier "Ordered") (Identifier "LessThanEqual")
    GreaterThan      -> NamedMethodReference (Identifier "Ordered") (Identifier "GreaterThan")
    GreaterThanEqual -> NamedMethodReference (Identifier "Ordered") (Identifier "GreaterThanEqual")
    And              -> NamedMethodReference (Identifier "Logical") (Identifier "Conjunction")
    Or               -> NamedMethodReference (Identifier "Logical") (Identifier "Disjunction")


untyped :: Either DesugarError Untyped
untyped = pure Untyped

desugarExpr :: Syntax.Expr -> Either DesugarError UntypedExpr
desugarExpr = \case
  Syntax.Subscript si es [Syntax.Singular e] ->
    Subscript (Metadata si) <$> desugarExpr es <*> desugarExpr e <*> untyped
  Syntax.Subscript si es [Syntax.Range e1 e2] ->
    Subslice (Metadata si) <$> desugarExpr es <*> (Range <$> desugarExpr e1 <*> desugarExpr e2) <*> untyped
  Syntax.Subscript si es [Syntax.RangeTo e] ->
    Subslice (Metadata si) <$> desugarExpr es <*> (RangeTo <$> desugarExpr e) <*> untyped
  Syntax.Subscript si es [Syntax.RangeFrom e] ->
    Subslice (Metadata si) <$> desugarExpr es <*> (RangeFrom <$> desugarExpr e) <*> untyped
  Syntax.Subscript si es (i:ir) ->
    desugarExpr (Syntax.Subscript si (Syntax.Subscript si es [i]) ir)

  Syntax.UnaryOp si op expr ->
    Application
    (Metadata si)
    (MethodReference (Metadata si) (methodForUnaryOperator op) Untyped)
    <$> desugarExpr expr
    <*> untyped
  Syntax.BinaryOp si op e1 e2 ->
    Application
    (Metadata si)
    <$> (Application
         (Metadata si)
         (MethodReference (Metadata si) (methodForBinaryOperator op) Untyped)
         <$> desugarExpr e1
         <*> untyped)
    <*> desugarExpr e2
    <*> untyped
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
    Tuple (Metadata si) <$> desugarExpr f <*> desugarExpr s <*> mapM desugarExpr r <*> untyped
  Syntax.If si c t f ->
    If (Metadata si) <$> desugarExpr c <*> desugarExpr t <*> desugarExpr f <*> untyped
  Syntax.Application si f [] ->
    NoArgApplication (Metadata si) <$> desugarExpr f <*> untyped
  Syntax.Application si f [p] ->
    Application (Metadata si) <$> desugarExpr f <*> desugarExpr p <*> untyped
  Syntax.Application si f ps ->
    Application (Metadata si) <$> desugarExpr (Syntax.Application si f (init ps))
                              <*> desugarExpr (last ps)
                              <*> untyped
  Syntax.Fn si [] b ->
    NoArgFn (Metadata si) <$> desugarExpr b <*> untyped
  Syntax.Fn si [arg] b ->
    Fn (Metadata si) (desugarNameBinding arg) <$> desugarExpr b <*> untyped
  Syntax.Fn si (arg:args) b ->
    Fn (Metadata si) (desugarNameBinding arg) <$> desugarExpr (Syntax.Fn si args b) <*> untyped
  Syntax.RecordInitializer si fields ->
    RecordInitializer (Metadata si) <$> mapM desugarFieldInitializer fields <*> return Untyped
  Syntax.MemberAccess si expr (Syntax.Symbol _ name) ->
    let memberAccess = NamedMemberAccess <$> desugarExpr expr <*> return name
    in MemberAccess (Metadata si) <$> memberAccess <*> untyped
  Syntax.MemberAccess si expr nonName -> do
    expr' <- desugarExpr expr
    nonName' <- desugarExpr nonName
    Left (InvalidMemberAccessExpression si expr' nonName')
  -- invalid, but can be handled anyway
  Syntax.Subscript _ a [] -> desugarExpr a
  Syntax.Let _ [] b -> desugarExpr b
  Syntax.Let _ [Syntax.LetPair si n e] b ->
    Let (Metadata si) (desugarNameBinding n) <$> desugarExpr e <*> desugarExpr b <*> untyped
  Syntax.Let si (Syntax.LetPair _ n e:bs) b ->
    Let (Metadata si) (desugarNameBinding n) <$> desugarExpr e <*> desugarExpr (Syntax.Let si bs b) <*> untyped
  Syntax.Slice si es ->
    Slice (Metadata si) <$> mapM desugarExpr es <*> untyped
  Syntax.Block si es ->
    Block (Metadata si) <$> mapM desugarExpr es <*> untyped
  Syntax.ProtocolMethodReference si (Syntax.Symbol _ protocol) (Syntax.Symbol _ method) ->
    return (MethodReference (Metadata si) (NamedMethodReference protocol method) Untyped)
  Syntax.ProtocolMethodReference si lhs rhs -> do
    lhs' <- desugarExpr lhs
    rhs' <- desugarExpr rhs
    Left (InvalidMemberAccessExpression si lhs' rhs')

-- temporary metadata for top level definitions, used for keeping track
-- of duplications and detecting missing terms for type signatures
data TempTopLevel = TempTop {
    tempType :: (SourceInfo, Maybe (TypeSignature SourceInfo)),
    -- whether this signature has a corresponding definition
    hasValue :: Bool
}

desugarFnShortHand :: Syntax.Definition
                   -> Either DesugarError (SourceInfo, Identifier, UntypedExpr)
desugarFnShortHand = \case
  Syntax.FnDefinition si name args body -> do
    expr <- desugarExpr (Syntax.Fn si args body)
    return (si, name, expr)
  Syntax.ValueDefinition si name expr   -> do
    expr' <- desugarExpr expr
    return (si, name, expr')

desugarTopLevel' :: PackageName
                 -> [Syntax.TopLevel]
                 -> Writer [DesugarError] ([ImportReference], [Definition])
desugarTopLevel' pkg top = do
  (is, scs, defs) <- foldM iter ([], Map.empty, []) top
  case filter (not . hasValue . snd) (Map.assocs scs) of
    [] -> return ()
    us -> mapM_ (tell . (:[]) . typeSigNoValErr) us
  return (is, defs)
  where
  iter :: ([ImportReference], Map.Map Identifier TempTopLevel, [Definition])
       -> Syntax.TopLevel
       -> Writer [DesugarError] ([ImportReference], Map.Map Identifier TempTopLevel, [Definition])
  iter (is, ts, defs) (Syntax.TopLevelDefinition topLevelDef) =
    case desugarFnShortHand topLevelDef of
      Left err -> do
        tell [err]
        return (is, ts, defs)
      Right (si, name, expr) ->
        let def = Definition (Metadata si) name (Map.lookup name ts >>= snd . tempType) expr
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
  iter (is, ts, defs) (Syntax.Implementation si protocolName typeSignature methods) =
    case mapM desugarMethodImpls methods of
      Left err -> do
        tell [err]
        return (is, ts, defs)
      Right methodImpls ->
        let impl = Implementation (Metadata si) protocolName typeSignature methodImpls
        in return (is, ts, defs ++ [impl])
    where
    desugarMethodImpls def = do
      (si', name, expr) <- desugarFnShortHand def
      return (MethodImplementation (Metadata si') name expr)
  newTypeSig name tsi msc =
    Map.insertWith (\_ old -> old) name (TempTop (tsi, msc) False)
  assignValue name si =
    -- if there's type signature, keep its location
    Map.insertWith (\_ (TempTop (_, sc') _) -> (TempTop (si, sc') True))
                     name (TempTop (si, Nothing) True)
  -- type signature doesn't have an assigned term
  typeSigNoValErr :: (Identifier, TempTopLevel) -> DesugarError
  typeSigNoValErr (n, TempTop (si, sc) _)
      = case sc of
          Just j_sc -> TypeSignatureWithoutDefinition si n j_sc
          -- if this happens, it's a bug in the compiler, rather than source code
          Nothing  -> error "Panic: type signature definition present without actual signature"
  -- type signature already defined
  typeSigReDefErr :: (Identifier, TempTopLevel) -> SourceInfo -> DesugarError
  typeSigReDefErr (n, TempTop (_, sc) _) si' = TypeSignatureRedefinition si' n sc

toEither :: (Eq v, Show v, Show e) => Writer [e] v -> Either [e] v
toEither w =
  case runWriter w of
    (a, []) -> Right a
    (_, es) -> Left es

desugarTopLevel :: PackageName -> [Syntax.TopLevel] -> Either [DesugarError] ([ImportReference], [Definition])
desugarTopLevel = (.) toEither . desugarTopLevel'

desugarPackage' :: Syntax.Package -> Writer [DesugarError] (UntypedPackage ImportReference)
desugarPackage' (Syntax.Package (Syntax.PackageDeclaration si name) definitions) = do
  (is, ds) <- desugarTopLevel' name definitions
  return (UntypedPackage (PackageDeclaration (Metadata si) name) is ds)

desugarPackage :: Syntax.Package -> Either [DesugarError] (UntypedPackage ImportReference)
desugarPackage p = case runWriter (desugarPackage' p) of
  (a, []) -> Right a
  (_, errs) -> Left errs

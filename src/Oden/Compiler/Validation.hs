{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}
module Oden.Compiler.Validation where

import           Oden.Core.Typed
import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Operator
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Traversal

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName        (QualifiedName (..))
import           Oden.SourceInfo
import           Oden.Type.Polymorphic
import           Oden.Type.Traversal

import           Oden.Compiler.LiteralEval

import           Control.Monad.RWS
import           Control.Monad.Except

import           Data.List
import qualified Data.Set                  as Set

data ValidationError = Redefinition SourceInfo Identifier
                     | ValueDiscarded TypedExpr
                     | DuplicatedRecordFieldName SourceInfo Identifier
                     | DivisionByZero TypedExpr
                     | NegativeSliceIndex TypedExpr
                     | InvalidSubslice SourceInfo TypedRange
                     | UnusedImport SourceInfo PackageName Identifier
                     deriving (Show, Eq, Ord)

data ValidationWarning = ValidationWarning -- There's no warnings defined yet.
                       deriving (Show, Eq, Ord)

data ValidationState = ValidationState { packageReferences :: Set.Set Identifier }

initState :: ValidationState
initState = ValidationState {
  packageReferences = Set.empty
}

type Validate = RWST
                (Set.Set Identifier)
                [ValidationWarning]
                ValidationState
                (Except ValidationError)

errorIfDefined :: Identifier -> Metadata SourceInfo -> Validate ()
errorIfDefined name (Metadata si) = do
  scope <- ask
  when (Set.member name scope) $
    throwError (Redefinition si name)

withIdentifier :: Identifier -> Validate a -> Validate a
withIdentifier = local . Set.insert

addPackageReference :: Identifier -> Validate ()
addPackageReference name = do
  refs <- gets packageReferences
  modify (\s -> s { packageReferences = Set.insert name refs })

validateSliceIndex :: TypedExpr -> Validate ()
validateSliceIndex e = do
  validateExpr e
  case evaluate e of
    Just (Int n) | n < 0 -> throwError $ NegativeSliceIndex e
    _ -> return ()


validateRange :: TypedRange -> Validate ()
validateRange r@(Range e1 e2) = do
  validateSliceIndex e1
  validateSliceIndex e2
  case (evaluate e1, evaluate e2) of
    (Just (Int v1), Just (Int v2)) |
      v1 > v2 -> throwError $ InvalidSubslice (getSourceInfo e1) r
    _ -> return ()
validateRange (RangeTo e) =
  validateSliceIndex e
validateRange (RangeFrom e) =
  validateSliceIndex e

validateExpr :: TypedExpr -> Validate ()
validateExpr = void . traverseExpr identityTraversal { onExpr = onExpr'
                                                     , onMemberAccess = onMemberAccess'
                                                     }
  where
  onExpr' expr = case expr of
    Subscript _ _ i _ -> do
      validateSliceIndex i
      return $ Just expr
    Subslice _ _ r _ -> do
      validateRange r
      return $ Just expr
    BinaryOp _ Divide _ rhs _ -> do
      when (evaluate rhs == Just (Int 0)) $
        throwError $ DivisionByZero expr
      return $ Just expr
    Fn _ (NameBinding si name) body _ ->
      onBindingAndExpression si name body
    Let _ (NameBinding si name) _ body _ ->
      onBindingAndExpression si name body
    Block _ exprs _ -> do
      mapM_ warnOnDiscarded (init exprs)
      return $ Just expr
      where
      warnOnDiscarded :: TypedExpr -> Validate ()
      warnOnDiscarded e = case typeOf e of
        TCon _ (FQN [] (Identifier "unit")) -> return ()
        _                                   -> throwError (ValueDiscarded e)
    _ -> return Nothing

  onBindingAndExpression si name innerExpr = do
    errorIfDefined name si
    withIdentifier name (onExpr' innerExpr)

  onMemberAccess' p@(PackageMemberAccess alias _) = do
    addPackageReference alias
    return p
  onMemberAccess' a = return a

repeated :: [(Identifier, Type)] -> [(Identifier, Type)]
repeated fields = snd (foldl check (Set.empty, []) fields)
  where
  check (names, fields') f@(n, _)
    | n `Set.member` names = (names, fields'++ [f])
    | otherwise            = (Set.insert n names, fields')

validateType :: Type -> Validate ()
validateType = void . traverseType onType'
  where
  hasDuplicateFields = null . repeated . sortOn fst
  onType' = \case
    r@(RExtension _ name _ _) | hasDuplicateFields (rowToList r) ->
        throwError (DuplicatedRecordFieldName (getSourceInfo r) name)
    t -> return t

validatePackage :: TypedPackage -> Validate ()
validatePackage (TypedPackage _ imports definitions) = do
  -- Validates all definitions and expressions recursively. Also collects usage
  -- of imported packages.
  validateDefs definitions
  -- When package usage is collected we can validate the imports.
  mapM_ validateImport imports
  where
  validateImport (ImportedPackage (Metadata sourceInfo) alias (TypedPackage (PackageDeclaration _ pkg) _ _)) = do
    refs <- gets packageReferences
    unless (alias `Set.member` refs) $
      throwError (UnusedImport sourceInfo pkg alias)
  validateDefs (Definition si name (_, expr):defs) = do
    errorIfDefined name si
    withIdentifier name $ do
      validateExpr expr
      validateDefs defs
  validateDefs (TypeDefinition _ (FQN _ n) _ type':defs) = do
    validateType type'
    withIdentifier n (validateDefs defs)
  validateDefs (Implementation _ (ProtocolImplementation _ _ methods) : defs) = do
    mapM_ validateMethod methods
    validateDefs defs
    where
    validateMethod (MethodImplementation _ _ expr) =
      validateExpr expr
  validateDefs (_:defs) = validateDefs defs
  validateDefs [] = return ()

validate :: TypedPackage -> Either ValidationError [ValidationWarning]
validate pkg =
  runExcept (snd <$> evalRWST (validatePackage pkg) Set.empty initState)

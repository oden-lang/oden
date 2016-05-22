{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE FlexibleContexts #-}
module Oden.Compiler.Validation.Untyped where

import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.Traversal
import           Oden.Core.Untyped

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName        (QualifiedName (..))
import           Oden.SourceInfo

import           Control.Monad.RWS
import           Control.Monad.Except

import qualified Data.Set                  as Set

data ValidationError
  = Redefinition SourceInfo Identifier
  | DuplicatedRecordFieldName SourceInfo Identifier
  deriving (Show, Eq, Ord)

data ValidationWarning = ValidationWarning -- There's no warnings defined yet.
                       deriving (Show, Eq, Ord)

data ValidationState = ValidationState

initState :: ValidationState
initState = ValidationState {}

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


validateExpr :: UntypedExpr -> Validate ()
validateExpr = void . traverseExpr identityTraversal { onExpr = onExpr'
                                                     }
  where
  onExpr' expr = case expr of
    Fn _ (NameBinding si name) body _ ->
      onBindingAndExpression si name body
    Let _ (NameBinding si name) _ body _ ->
      onBindingAndExpression si name body
    _ -> return Nothing

  onBindingAndExpression si name innerExpr = do
    errorIfDefined name si
    withIdentifier name (onExpr' innerExpr)

validatePackage :: UntypedPackage ImportReference -> Validate ()
validatePackage (UntypedPackage _ _ definitions) =
  -- Validates all definitions and expressions recursively.
  validateDefs definitions
  where
  validateDefs =
    \case
      Definition si name _ expr : defs -> do
        errorIfDefined name si
        withIdentifier name $ do
          validateExpr expr
          validateDefs defs
      TypeDefinition _ (FQN _ n) _ _ : defs ->
        withIdentifier n (validateDefs defs)
      Implementation _ _ methods : defs -> do
        mapM_ validateMethod methods
        validateDefs defs
        where
        validateMethod (MethodImplementation _ _ expr) =
          validateExpr expr
      _ : defs -> validateDefs defs
      [] -> return ()

validate :: UntypedPackage ImportReference
         -> Either ValidationError [ValidationWarning]
validate pkg =
  runExcept (snd <$> evalRWST (validatePackage pkg) Set.empty initState)


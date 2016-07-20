{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
-- | Resolves protocol method usage to implementation if there's a single
-- possible resolution that matches. Note that we only make local protocol
-- implementations available for now.
module Oden.Compiler.Resolution (
  ResolutionError(..),
  resolveInPackage
  ) where

import           Oden.Core.Definition
import           Oden.Core.Expr                   (Expr (..), NameBinding (..),
                                                   mapType, typeOf)
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Traversal
import           Oden.Core.Typed

import           Oden.Compiler.Environment
import           Oden.Compiler.Instantiate
import           Oden.Compiler.TypeEncoder

import           Oden.Infer.Unification

import           Oden.Environment                 as Environment
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Control.Monad.Except
import           Control.Monad.Trans.RWS

import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Set                         (Set)
import qualified Data.Set                         as Set


data ResolutionError
  = NotInScope Identifier
  | NoMatchingImplementationInScope SourceInfo ProtocolName Type [ProtocolImplementation TypedExpr]
  | MultipleMatchingImplementationsInScope SourceInfo [ProtocolImplementation TypedExpr]
  | ResolutionInstantiateError InstantiateError
  deriving (Show, Eq, Ord)

type ResolveLog = Set ProtocolConstraint

data ResolveState = ResolveState { instanceNames :: Map (QualifiedName, Type) Identifier
                                 , instances     :: Map (QualifiedName, Type) TypedDefinition }


initialState :: ResolveState
initialState = ResolveState { instanceNames = Map.empty
                            , instances = Map.empty
                            }

type Resolve = RWST
               CompileEnvironment
               ResolveLog
               ResolveState
               (Except ResolutionError)


resolveImplementation :: ProtocolConstraint -> Resolve (Maybe (ProtocolImplementation TypedExpr))
resolveImplementation =
  \case
    ProtocolConstraint _ _ TVar{} -> return Nothing
    ProtocolConstraint (Metadata si) protocolName' type' -> do
      allImpls <- asks (Set.toList . implementations)
      case filter implements allImpls of
        []     -> throwError (NoMatchingImplementationInScope si protocolName' type' allImpls)
        [impl] -> return (Just impl)
        impls  -> throwError (MultipleMatchingImplementationsInScope si impls)
      where
      implements (ProtocolImplementation _ implName implType _) =
        implName == protocolName' && implType `unifiesWith` type'


resolveInMethodImplementation :: MethodImplementation TypedExpr
                              -> Resolve (MethodImplementation TypedExpr)
resolveInMethodImplementation (MethodImplementation si method expr) =
  MethodImplementation si method <$> resolveInExpr' expr


resolveInImplementation :: ProtocolImplementation TypedExpr
                        -> Resolve (ProtocolImplementation TypedExpr)
resolveInImplementation (ProtocolImplementation si protocol implHead methods) =
  ProtocolImplementation si protocol implHead <$> mapM resolveInMethodImplementation methods


findMethod :: ProtocolImplementation TypedExpr
           -> MethodName
           -> Resolve (MethodImplementation TypedExpr)
findMethod (ProtocolImplementation _ _ _ methods) name =
  case filter matchesName methods of
    [method] -> return method
    _ -> error ("expected method to be present in implementation: " ++ show name)
  where
  matchesName (MethodImplementation _ methodName _) =
    methodName == name


lookupIn :: CompileEnvironment -> Identifier -> Resolve Binding
lookupIn env identifier =
  case Environment.lookup identifier env of
      Nothing      -> throwError $ NotInScope identifier
      Just binding -> return binding


addInstanceName :: (QualifiedName, Type) -> Identifier -> Resolve ()
addInstanceName key name =
  modify (\s -> s { instanceNames = Map.insert key name (instanceNames s) })

addInstance :: (QualifiedName, Type)
            -> TypedDefinition
            -> Resolve ()
addInstance key def =
  modify (\s -> s { instances = Map.insert key def (instances s) })

getResolvedIn :: CompileEnvironment
              -> Identifier
              -> Set ProtocolConstraint
              -> Type
              -> Resolve Identifier
getResolvedIn env ident constraints t = do
  binding <- lookupIn env ident
  case binding of
    PackageBinding{} -> throwError $ NotInScope ident
    DefinitionBinding definition ->
      case definition of
        ForeignDefinition{} -> return ident
        Definition _ fqn (sc, pe) | isPolymorphic sc -> do
          let key = (fqn, t)
          is <- gets instanceNames
          case Map.lookup key is of
            Just name -> return name
            Nothing -> instantiateDefinition key constraints pe
        Definition _ (FQN _ n) _ -> return n
        -- Types cannot be referred to at this stage.
        TypeDefinition{} -> throwError $ NotInScope ident
        -- Protocols cannot be referred to at this stage.
        ProtocolDefinition{} -> throwError $ NotInScope ident
        -- Implementations cannot be referred to at this stage.
        Implementation{} -> throwError $ NotInScope ident

    LetBinding (NameBinding _ _boundIdentifier) _expr ->
      error "oh oh, let binding"
      -- getMonomorphicLetBinding boundIdentifier t (typeOf expr)
    FunctionArgument (NameBinding _ boundIdentifier) ->
      return boundIdentifier


instantiateDefinition :: (QualifiedName, Type)
                      -> Set ProtocolConstraint
                      -> TypedExpr
                      -> Resolve Identifier
instantiateDefinition key@(FQN pkgName name, type') constraints expr = do
  let identifier = Identifier ("__" ++ encodeTypeInstance (FQN pkgName name) type')
      exprWithoutConstraints = mapType (`dropConstraints` constraints) expr
  case instantiate exprWithoutConstraints type' of
    Left err -> throwError (ResolutionInstantiateError err)
    Right expr' -> do
      -- First add the name to the state to avoid endless loops for
      -- recursive functions that would resolve themselves.
      addInstanceName key identifier
      resolvedExpr <- resolveInExpr' expr'
      -- ... then add the actual instance.
      let originalType = typeOf expr'
          def = Definition
                (Metadata $ getSourceInfo expr')
                (FQN pkgName identifier)
                (Forall
                 (Metadata $ getSourceInfo originalType)
                 []
                 Set.empty
                 (typeOf resolvedExpr),
                 resolvedExpr)
      addInstance key def

      return identifier



resolveInExpr' :: TypedExpr -> Resolve TypedExpr
resolveInExpr' = traverseExpr traversal
  where
  traversal = Traversal { onExpr = onExpr'
                        , onType = return
                        , onMemberAccess = onMemberAccess'
                        , onNameBinding = return
                        , onMethodReference = onMethodReference' }
  onExpr' =
    \case
      Symbol meta name (TConstrained constraints underlying') -> do
        env <- ask
        resolvedName <- getResolvedIn env name constraints underlying'
        return (Just (Symbol meta resolvedName underlying'))
      _ -> return Nothing
  onMethodReference' si reference methodType =
    case reference of
      Unresolved protocolName' methodName constraint -> do
        impl <- resolveImplementation constraint
        case impl of
          Just impl' -> do
            method <- findMethod impl' methodName
            let constraints = Set.singleton constraint
            tell constraints
            -- As we have resolved an implementation for this constraint, we
            -- can remove it the from type.
            return (si,
                    Resolved protocolName' methodName method,
                    dropConstraints methodType constraints)
          Nothing ->
            return (si, reference, methodType)
      Resolved{} ->
        return (si, reference, methodType)
  onMemberAccess' = \case
    RecordFieldAccess expr identifier ->
      RecordFieldAccess <$> resolveInExpr' expr <*> return identifier
    PackageMemberAccess pkg member ->
      return (PackageMemberAccess pkg member)

shouldTryToResolve :: Scheme -> Bool
shouldTryToResolve (Forall _ _ constraints _) =
  null constraints || any shouldResolve constraints
  where
    shouldResolve (ProtocolConstraint _ _ TVar{}) = False
    shouldResolve ProtocolConstraint{} = True

resolveInDefinitions :: [TypedDefinition] -> Resolve [TypedDefinition]
resolveInDefinitions [] = return []
resolveInDefinitions (def:defs) =
  case def of
    Definition si name (scheme, expr) | shouldTryToResolve scheme -> do
      (resolvedExpr, resolvedConstraints) <- listen (resolveInExpr' expr)
      let scheme' = dropConstraints scheme resolvedConstraints
          unconstrainedExpr = mapType (`dropConstraints` resolvedConstraints) resolvedExpr
          unconstrainedDef =
            Definition si name (scheme', unconstrainedExpr)
      (:) unconstrainedDef <$> resolveInDefinitions defs
    Definition{} ->
      resolveInDefinitions defs
    ForeignDefinition{} ->
      (:) def <$> resolveInDefinitions defs
    TypeDefinition{} ->
      (:) def <$> resolveInDefinitions defs
    ProtocolDefinition{} ->
      (:) def <$> resolveInDefinitions defs
    Implementation si implementation -> do
      resolved <- resolveInImplementation implementation
      let updateEnv :: CompileEnvironment -> CompileEnvironment
          updateEnv = Environment.addImplementation resolved . Environment.removeImplementation implementation
      (:) (Implementation si resolved) <$> local updateEnv (resolveInDefinitions defs)


runResolve :: CompileEnvironment
           -> Resolve a
           -> Either ResolutionError (a, ResolveState, ResolveLog)
runResolve env action = runExcept (runRWST action env initialState)


resolveInPackage :: CompileEnvironment
                 -> TypedPackage
                 -> Either ResolutionError TypedPackage
resolveInPackage env (TypedPackage decl imports defs) = do
  (resolvedDefs, state', _) <- runResolve env (resolveInDefinitions defs)
  let allDefs = Map.elems (instances state') ++ resolvedDefs
  return (TypedPackage decl imports allDefs)

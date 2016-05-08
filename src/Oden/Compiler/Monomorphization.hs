-- | Generates monomorphic definitions from all usages of polymorphic
-- definitions. Generated names are based on which monomorphic types are used.
-- All references in are replaced with the generated names. This module also
-- does monomorpization for let-bound polymorphic values.
--
-- Any polymorphic definitions and let bindings not used will be ignored.
module Oden.Compiler.Monomorphization where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Map                  as Map hiding (map)
import           Data.Set                  as Set hiding (map)

import           Oden.Compiler.Environment
import           Oden.Compiler.Instantiate
import           Oden.Compiler.TypeEncoder

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Monomorphed     as Monomorphed
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Typed           as Typed

import           Oden.Identifier
import           Oden.Environment                as Environment
import           Oden.Metadata
import           Oden.Predefined
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic     as Mono
import qualified Oden.Type.Polymorphic     as Poly

data MonomorphError   = NotInScope Identifier
                      | UnexpectedPolyType SourceInfo Poly.Type
                      | MonomorphInstantiateError InstantiateError
                      deriving (Show, Eq, Ord)

-- | Represents a reference to a let-bound identifier and the monomorphic type it was
-- inferred to.
data LetReference = LetReference Identifier Mono.Type Identifier
                 deriving (Show, Eq, Ord)

-- | A let-bound expression instantiated to a monomorphic type with which it
-- was used.
data LetInstance = LetInstance (Metadata SourceInfo) NameBinding MonoTypedExpr

data MonomorphState = MonomorphState { instanceNames :: Map (Identifier, Mono.Type) Identifier
                                     , instances   :: [InstantiatedDefinition]
                                     , monomorphed :: Map Identifier MonomorphedDefinition
                                     }

-- | The monomorphization monad stack that keeps track of bindings, references
-- and what is has monomorphed already.
type Monomorph a = RWST CompileEnvironment
                        (Map Identifier LetReference)
                        MonomorphState
                        (Except MonomorphError)
                        a

lookupIn :: CompileEnvironment -> Identifier -> Monomorph Binding
lookupIn env identifier =
  case Environment.lookup identifier env of
      Nothing             -> throwError $ NotInScope identifier
      Just binding        -> return binding

withIdentifier :: Identifier -> Binding -> Monomorph a -> Monomorph a
withIdentifier identifier binding = local (`extend` (identifier, binding))

addInstanceName :: (Identifier, Mono.Type) -> Identifier -> Monomorph ()
addInstanceName key name =
  modify (\s -> s { instanceNames = Map.insert key name (instanceNames s) })

addInstance :: InstantiatedDefinition -> Monomorph ()
addInstance inst =
  modify (\s -> s { instances = instances s ++ [inst] })

addMonomorphed :: Identifier -> MonomorphedDefinition -> Monomorph ()
addMonomorphed identifier def =
  modify (\s -> s { monomorphed = Map.insert identifier def (monomorphed s) })

instantiateMethod :: Poly.Protocol
                  -> Poly.ProtocolMethod
                  -> MethodImplementation TypedExpr
                  -> Mono.Type
                  -> Monomorph Identifier
instantiateMethod protocol method (MethodImplementation _ _ expr) monoType = do
  let identifier = Identifier (encodeMethodInstance
                               (Poly.protocolName protocol)
                               (Poly.protocolMethodName method)
                               monoType)
  case instantiate expr monoType of
    Left err -> throwError (MonomorphInstantiateError err)
    Right monoExpr -> do
      addInstanceName (identifier, monoType) identifier
      me <- monomorph monoExpr
      addInstance (InstantiatedMethod (Metadata $ getSourceInfo monoType) identifier me)
      return identifier

instantiateDefinition :: (Identifier, Mono.Type)
                      -> TypedExpr
                      -> Monomorph Identifier
instantiateDefinition key@(pn, t) pe = do
  let identifier = Identifier (encodeTypeInstance pn t)
  case instantiate pe t of
    Left err -> throwError (MonomorphInstantiateError err)
    Right expr -> do
      -- First add the name to the state to avoid endless loops for polymorphic
      -- recursive functions that would monomorph themselves.
      addInstanceName key identifier
      me <- monomorph expr
      -- ... then add the actual instance.
      addInstance (InstantiatedDefinition pn (Metadata $ getSourceInfo t) identifier me)
      return identifier

getMonomorphicIn :: CompileEnvironment -> Identifier -> Mono.Type -> Monomorph Identifier
getMonomorphicIn env ident t = do
  binding <- lookupIn env ident
  case binding of
    PackageBinding{} -> throwError $ NotInScope ident
    DefinitionBinding definition ->
      case definition of
        ForeignDefinition{} -> return ident
        Definition _ _ (sc, pe) | Poly.isPolymorphic sc -> do
          let key = (ident, t)
          is <- gets instanceNames
          case Map.lookup key is of
            Just name -> return name
            Nothing -> instantiateDefinition key pe
        Definition _ pn _ -> return pn
        -- Types cannot be referred to at this stage.
        TypeDefinition{} -> throwError $ NotInScope ident
        -- Protocols cannot be referred to at this stage.
        ProtocolDefinition{} -> throwError $ NotInScope ident
        -- Implementations cannot be referred to at this stage.
        Implementation{} -> throwError $ NotInScope ident

    LetBinding (NameBinding _ boundIdentifier) expr ->
      getMonomorphicLetBinding boundIdentifier t (typeOf expr)
    FunctionArgument (NameBinding _ boundIdentifier) ->
      return boundIdentifier

getMonomorphicLetBinding :: Identifier
                         -> Mono.Type
                         -> Poly.Type
                         -> Monomorph Identifier
getMonomorphicLetBinding identifier mt pt | not (Poly.isPolymorphicType pt) = do
  tell (Map.singleton identifier (LetReference identifier mt identifier))
  return identifier
getMonomorphicLetBinding identifier mt _ = do
  let encoded = Identifier (encodeTypeInstance identifier mt)
  tell (Map.singleton identifier (LetReference identifier mt encoded))
  return encoded

toMonomorphic :: Metadata SourceInfo -> Poly.Type -> Monomorph Mono.Type
toMonomorphic (Metadata si) pt =
  either
  (const $ throwError (UnexpectedPolyType si pt))
  return
  (Poly.toMonomorphic pt)

getMonoType :: TypedExpr -> Monomorph Mono.Type
getMonoType e = toMonomorphic (Metadata $ getSourceInfo e) (typeOf e)

-- | Return a monomorphic version of a polymorphic expression.
monomorph :: TypedExpr -> Monomorph MonoTypedExpr
monomorph e = case e of
  Symbol si ident _ -> do
    env <- ask
    mt <- getMonoType e
    m <- getMonomorphicIn env ident mt
    return (Symbol si m mt)

  Subscript sourceInfo sliceExpr indexExpr polyType ->
    Subscript sourceInfo <$> monomorph sliceExpr
                         <*> monomorph indexExpr
                         <*> toMonomorphic sourceInfo polyType

  Subslice sourceInfo sliceExpr (Range lowerExpr upperExpr) polyType ->
    Subslice sourceInfo <$> monomorph sliceExpr
                        <*> (Range <$> monomorph lowerExpr <*> monomorph upperExpr)
                        <*> toMonomorphic sourceInfo polyType

  Subslice sourceInfo sliceExpr (RangeTo indexExpr) polyType ->
    Subslice sourceInfo <$> monomorph sliceExpr
                        <*> (RangeTo <$> monomorph indexExpr)
                        <*> toMonomorphic sourceInfo polyType

  Subslice sourceInfo sliceExpr (RangeFrom indexExpr) polyType ->
    Subslice sourceInfo <$> monomorph sliceExpr
                        <*> (RangeFrom <$> monomorph indexExpr)
                        <*> toMonomorphic sourceInfo polyType

  UnaryOp si o e1 _ -> do
    mt <- getMonoType e
    me <- monomorph e1
    return (UnaryOp si o me mt)

  BinaryOp si o e1 e2 _ -> do
    mt <- getMonoType e
    me1 <- monomorph e1
    me2 <- monomorph e2
    return (BinaryOp si o me1 me2 mt)

  Application si f p _ -> do
    mt <- getMonoType e
    mf <- monomorph f
    mp <- monomorph p
    return (Application si mf mp mt)

  NoArgApplication si f _ -> do
    mt <- getMonoType e
    mf <- monomorph f
    return (NoArgApplication si mf mt)

  ForeignFnApplication si f ps _ -> do
    mt <- getMonoType e
    mf <- monomorph f
    mps <- mapM monomorph ps
    return (ForeignFnApplication si mf mps mt)

  Fn si param@(NameBinding _ identifier) b _ -> do
    mt <- getMonoType e
    mb <- withIdentifier identifier (FunctionArgument param) (monomorph b)
    return (Fn si param mb mt)

  NoArgFn si b _ -> do
    mt <- getMonoType e
    mb <- monomorph b
    return (NoArgFn si mb mt)

  Slice si es _ -> do
    mt <- getMonoType e
    mes <- mapM monomorph es
    return (Slice si mes mt)

  Block si es _ -> do
    mt <- getMonoType e
    mes <- mapM monomorph es
    return (Block si mes mt)

  Literal si l _ -> do
    mt <- getMonoType e
    return (Literal si l mt)

  Tuple si f s r _ -> do
    mt <- getMonoType e
    mf <- monomorph f
    ms <- monomorph s
    mr <- mapM monomorph r
    return (Tuple si mf ms mr mt)

  If si cond then' else' _ -> do
    mt <- getMonoType e
    mCond <- monomorph cond
    mThen <- monomorph then'
    mElse <- monomorph else'
    return (If si mCond mThen mElse mt)

  Let si b@(NameBinding bsi identifier) expr body _ -> do
    (mBody, allRefs) <- listen (withIdentifier identifier (LetBinding b expr) (monomorph body))
    let (refs, other) = Map.partition (isReferenceTo b) allRefs
    tell other
    insts <- mapM (monomorphReference expr si bsi) (Map.elems refs)
    return (unwrapLetInstances insts mBody)
    where
    isReferenceTo :: NameBinding -> LetReference -> Bool
    isReferenceTo (NameBinding _ identifier') (LetReference letted _ _) =
      identifier' == letted

  RecordInitializer si fields _ -> do
    monoType <- getMonoType e
    monoFields <- mapM monomorphField fields
    return (RecordInitializer si monoFields monoType)
    where
    monomorphField (FieldInitializer fsi label expr) =
      FieldInitializer fsi label <$> monomorph expr

  MemberAccess si access polyType ->
    case access of
      Typed.RecordFieldAccess expr name -> do
        monoType <- getMonoType e
        monoExpr <- monomorph expr
        return (MemberAccess si (Monomorphed.RecordFieldAccess monoExpr name) monoType)

      Typed.PackageMemberAccess pkgAlias name -> do
        env <- ask
        monoType <- toMonomorphic si polyType
        binding <- lookupIn env pkgAlias
        case binding of
          PackageBinding _ _ pkgEnv -> do
            m <- getMonomorphicIn pkgEnv name monoType
            return (MemberAccess si (Monomorphed.PackageMemberAccess pkgAlias m) monoType)
          _ -> error "cannot access member in non-existing package"

  MethodReference si reference methodType ->
    case reference of
      Unresolved _protocol method ->
        error (show method)
      Resolved protocol method methodImpl -> do
        monoType <- toMonomorphic si methodType
        name <- instantiateMethod protocol method methodImpl monoType
        return (Symbol si name monoType)

-- Given a let-bound expression and a reference to that binding, create a
-- monomorphic instance of the let-bound expression.
monomorphReference :: TypedExpr
                   -> Metadata SourceInfo -- Let expression source info.
                   -> Metadata SourceInfo -- Let binding source info.
                   -> LetReference
                   -> Monomorph LetInstance

-- The let-bound value is monomorphic and does not need to be instantiated.
monomorphReference e letSourceInfo bindingSourceInfo (LetReference identifier _ _)
  | not (Poly.isPolymorphicType (typeOf e)) = do
    me <- monomorph e
    return (LetInstance letSourceInfo (NameBinding bindingSourceInfo identifier) me)

-- The let-bound value is polymorphic and must be instantiated.
monomorphReference e letSourceInfo bindingSourceInfo (LetReference _ mt mn) =
  case instantiate e mt of
    Left err -> throwError (MonomorphInstantiateError err)
    Right expr -> do
      me <- monomorph expr
      return (LetInstance letSourceInfo (NameBinding bindingSourceInfo mn) me)

-- | Creates nested let expressions for each let instance around the body
-- expression.
unwrapLetInstances :: [LetInstance]
                   -> MonoTypedExpr -- Body expression.
                   -> MonoTypedExpr
unwrapLetInstances [] body = body
unwrapLetInstances (LetInstance si mn me:is) body =
  Let si mn me (unwrapLetInstances is body) (typeOf body)

-- | Monomorphs definitions and keeps results in the state.
monomorphDefinitions :: [TypedDefinition]
                     -> Monomorph ()
monomorphDefinitions [] = return ()
monomorphDefinitions (d@(Definition si identifier (Poly.Forall _ _ _ st, expr)) : defs) = do
  case Poly.toMonomorphic st of
    Left _ -> return ()
    Right mt -> do
      mExpr <- monomorph expr
      addMonomorphed identifier (MonomorphedDefinition si identifier mt mExpr)
  -- Monomorph rest of the definitions with this definitions identifier in
  -- the environment.
  local (`extend` (identifier, DefinitionBinding d)) (monomorphDefinitions defs)
-- Type definitions are not monomorphed or generated to output code, so ignore.
monomorphDefinitions (TypeDefinition{} : defs) =
  monomorphDefinitions defs
-- Foreign definitions are are already monomorphic and not generated to out
-- code, so ignore.
monomorphDefinitions (ForeignDefinition{} : defs) =
  monomorphDefinitions defs
-- Protocol definitions are not monomorphed or generated to output code, so
-- ignore.
monomorphDefinitions (ProtocolDefinition{} : defs) =
  monomorphDefinitions defs
-- Implementations are not monomorphed or generated to output code, so
-- ignore.
monomorphDefinitions (Implementation{} : defs) =
  monomorphDefinitions defs

-- | Monomorphs a package and returns the complete package with instantiated
-- and monomorphed definitions.
monomorphPackage :: TypedPackage
                 -> Either MonomorphError MonomorphedPackage
monomorphPackage self@(TypedPackage pkgDecl imports definitions) = do
  let environment = fromPackage universe `merge` fromPackage self `merge` fromPackages imports
  let st = MonomorphState { instanceNames = Map.empty
                          , instances = []
                          , monomorphed = Map.empty
                          }
  (_, s, _) <- runExcept $ runRWST (monomorphDefinitions definitions) environment st
  let is = Set.fromList (instances s)
      ms = Set.fromList (Map.elems (monomorphed s))
  return (MonomorphedPackage pkgDecl imports is ms)

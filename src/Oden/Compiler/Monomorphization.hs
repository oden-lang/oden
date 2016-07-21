-- | Generates monomorphic definitions from all usages of polymorphic
-- definitions. Generated names are based on which monomorphic types are used.
-- All references in are replaced with the generated names. This module also
-- does monomorpization for let-bound polymorphic values.
--
-- Any polymorphic definitions and let bindings not used will be ignored.
{-# LANGUAGE LambdaCase #-}
module Oden.Compiler.Monomorphization where

import           Control.Monad.Except
import           Control.Monad.RWS

import           Data.Map                         as Map hiding (map)
import           Data.Set                         as Set hiding (map)

import           Oden.Compiler.Environment
import           Oden.Compiler.Instantiate
import           Oden.Compiler.NameEncoder

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Monomorphed            as Monomorphed
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Typed                  as Typed

import           Oden.Environment                 as Environment
import           Oden.Identifier
import           Oden.QualifiedName
import           Oden.Metadata

import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic            as Mono
import qualified Oden.Type.Polymorphic            as Poly


data MonomorphError
  = NotInScope Identifier
  | UnexpectedPolyType SourceInfo Poly.Type
  | MonomorphInstantiateError InstantiateError
  | UnresolvedMethodReference SourceInfo Poly.ProtocolName Poly.MethodName Poly.ProtocolConstraint
  deriving (Show, Eq, Ord)

-- | Represents a reference to a let-bound identifier and the monomorphic type it was
-- inferred to.
data LetReference = LetReference Identifier Poly.Type Identifier
                 deriving (Show, Eq, Ord)

-- | A let-bound expression instantiated to a monomorphic type with which it
-- was used.
data LetInstance = LetInstance (Metadata SourceInfo) NameBinding MonoTypedExpr

-- | The source of an instance, for differentiating between regular polymorphic definitions
-- and instantiated methods.
data InstanceSource
  = DefinitionInstanceSource QualifiedName Poly.Type
  | MethodInstanceSource QualifiedName Identifier Poly.Type
  deriving (Show, Eq, Ord)

data MonomorphState = MonomorphState { instanceNames    :: Map InstanceSource Identifier
                                     , instances        :: [InstantiatedDefinition]
                                     , monomorphedNames :: Map QualifiedName Identifier
                                     , monomorphed      :: Map QualifiedName MonomorphedDefinition
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


addInstanceName :: InstanceSource -> Identifier -> Monomorph ()
addInstanceName source name =
  modify (\s -> s { instanceNames = Map.insert source name (instanceNames s) })


addInstance :: InstantiatedDefinition -> Monomorph ()
addInstance inst =
  modify (\s -> s { instances = instances s ++ [inst] })

  
addMonomorphedName :: QualifiedName -> Identifier -> Monomorph ()
addMonomorphedName original name =
  modify (\s -> s { monomorphedNames = Map.insert original name (monomorphedNames s) })


addMonomorphed :: QualifiedName -> MonomorphedDefinition -> Monomorph ()
addMonomorphed fqn def =
  modify (\s -> s { monomorphed = Map.insert fqn def (monomorphed s) })


instantiateMethod :: Poly.ProtocolName
                  -> Poly.MethodName
                  -> TypedExpr
                  -> Poly.Type
                  -> Monomorph (Mono.Type, Identifier)
instantiateMethod protocolName methodName expr instanceType =
  case instantiate expr instanceType of
    Left err -> throwError (MonomorphInstantiateError err)
    Right instanceExpr -> do
      let source = MethodInstanceSource protocolName methodName (typeOf instanceExpr)
          identifier = Identifier ("__" ++ encodeMethodInstance protocolName methodName (typeOf instanceExpr))
      addInstanceName source identifier
      me <- monomorph instanceExpr
      addInstance (InstantiatedMethod (Metadata $ getSourceInfo instanceType) identifier me)
      return (typeOf me, identifier)


instantiateDefinition :: QualifiedName
                      -> Poly.Type
                      -> TypedExpr
                      -> Monomorph Identifier
instantiateDefinition fqn t expr =
  case instantiate expr t of
    Left err -> throwError (MonomorphInstantiateError err)
    Right instanceExpr -> do
      let instantiatedType = typeOf instanceExpr
          source = DefinitionInstanceSource fqn instantiatedType
          identifier = Identifier ("__" ++ encodeTypeInstance fqn instantiatedType)
      -- First add the name to the state to avoid endless loops for polymorphic
      -- recursive functions that would instantiate themselves.
      addInstanceName source identifier
      me <- monomorph instanceExpr
      -- ... then add the actual instance.
      addInstance (InstantiatedDefinition fqn (Metadata $ getSourceInfo t) identifier me)
      return identifier

  
-- | An identifier after the 'flattening' process, i.e. when imported native definitions gets
-- insert into the package and every definition gets a name based on it's fully qualified name.
data MonomorphicIdentifier
  = FlattenedIdentifier Identifier
  | LocalIdentifier Identifier
  | ForeignIdentifier QualifiedName


findInstanceName :: InstanceSource -> Monomorph (Maybe Identifier)
findInstanceName source = Map.lookup source <$> gets instanceNames


getMonomorphicIn :: CompileEnvironment
                 -> Identifier
                 -> Poly.Type
                 -> Monomorph MonomorphicIdentifier
getMonomorphicIn env ident t = do
  binding <- lookupIn env ident
  case binding of
    PackageBinding{} -> throwError $ NotInScope ident
    DefinitionBinding definition ->
      case definition of
        ForeignDefinition _ definitionName _ ->
          return (ForeignIdentifier definitionName)
        Definition _ definitionName (sc, pe) | Poly.isPolymorphic sc -> do
          res <- findInstanceName (DefinitionInstanceSource definitionName t)
          case res of
            Just name ->
              return (FlattenedIdentifier name)
            Nothing ->
              FlattenedIdentifier <$> instantiateDefinition definitionName t pe
        Definition{} -> do
          res <- monomorphDefinition definition
          case res of
            Just name -> return (FlattenedIdentifier name)
            Nothing -> throwError $ NotInScope ident
        -- Types cannot be referred to at this stage.
        TypeDefinition{} -> throwError $ NotInScope ident
        -- Protocols cannot be referred to at this stage.
        ProtocolDefinition{} -> throwError $ NotInScope ident
        -- Implementations cannot be referred to at this stage.
        Implementation{} -> throwError $ NotInScope ident

    LetBinding (NameBinding _ boundIdentifier) expr ->
      LocalIdentifier <$> getMonomorphicLetBinding boundIdentifier t (typeOf expr)
    FunctionArgument (NameBinding _ boundIdentifier) ->
      return (LocalIdentifier boundIdentifier)


getMonomorphicMethod :: QualifiedName
                     -> Identifier
                     -> TypedExpr
                     -> Poly.Type
                     -> Monomorph (Identifier, Mono.Type)
getMonomorphicMethod protocolName' methodName expr polyType = do
  res <- findInstanceName (MethodInstanceSource protocolName' methodName polyType)
  case res of
    Just name -> do
      monoType <- getMonoType expr
      return (name, monoType)
    Nothing -> do
      (monoType, name) <- instantiateMethod protocolName' methodName expr polyType
      return (name, monoType)


getMonomorphicLetBinding :: Identifier
                         -> Poly.Type
                         -> Poly.Type
                         -> Monomorph Identifier
getMonomorphicLetBinding identifier mt pt | not (Poly.isPolymorphicType pt) = do
  tell (Map.singleton identifier (LetReference identifier mt identifier))
  return identifier
getMonomorphicLetBinding identifier mt _ = do
  let encoded = Identifier ("__" ++ encodeUnqualifiedTypeInstance identifier mt)
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
    m <- getMonomorphicIn env ident (typeOf e)
    case m of
      FlattenedIdentifier n -> return (Symbol si n mt)
      LocalIdentifier n -> return (Symbol si n mt)
      ForeignIdentifier _ -> return (Symbol si ident mt)

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
        binding <- lookupIn env pkgAlias
        case binding of
          PackageBinding _ _ pkgEnv -> do
            m <- getMonomorphicIn pkgEnv name polyType
            monoType <- getMonoType e
            case m of
              FlattenedIdentifier n ->
                return (Symbol si n monoType)
              LocalIdentifier n ->
                return (Symbol si n monoType)
              ForeignIdentifier _ ->
                return (MemberAccess si (Monomorphed.PackageMemberAccess pkgAlias name) monoType)
          _ -> error "cannot access member in non-existing package"

  MethodReference si reference methodType ->
    case reference of
      Unresolved protocolName methodName constraint ->
        throwError (UnresolvedMethodReference (unwrap si) protocolName methodName constraint)
      Resolved protocolName' methodName (MethodImplementation _ _ expr)
        | shouldInline expr -> monomorph expr
        | otherwise -> do
            (name, monoType) <- getMonomorphicMethod protocolName' methodName expr methodType
            return (Symbol si name monoType)
    where
    shouldInline =
      \case
        Foreign{} -> True
        Literal{} -> True
        _         -> False

  Foreign si f t -> do
    mt <- toMonomorphic si t
    return (Foreign si f mt)


-- | Given a let-bound expression and a reference to that binding, create a
-- monomorphic instance of the let-bound expression.
monomorphReference :: TypedExpr
                   -> Metadata SourceInfo -- Let expression source info.
                   -> Metadata SourceInfo -- Let binding source info.
                   -> LetReference
                   -> Monomorph LetInstance
monomorphReference e letSourceInfo bindingSourceInfo =
  \case
    -- The let-bound value is monomorphic and does not need to be instantiated.
    LetReference identifier _ _
      | not (Poly.isPolymorphicType (typeOf e)) -> do
          me <- monomorph e
          return (LetInstance letSourceInfo (NameBinding bindingSourceInfo identifier) me)

    -- The let-bound value is polymorphic and must be instantiated.
    LetReference _ mt mn ->
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


encodeDefinitionName :: QualifiedName -> Identifier
encodeDefinitionName =
  \case
    FQN _ (Identifier "main") ->
      Identifier "main"
    fqn ->
      Identifier ("__" ++ encodeQualifiedName fqn)

      
-- | Recursively monomorph the definition if it has a monomorphic type. For definitions
-- with polymorphic types the result will be 'Nothing'.
monomorphDefinition :: TypedDefinition
                    -> Monomorph (Maybe Identifier)
monomorphDefinition def =
  case def of
    Definition si fqn (Poly.Forall _ _ _ st, expr) ->
      case Poly.toMonomorphic st of
        Left _ -> return Nothing
        Right mt -> do
          -- Check if this definition has already been monomorphed and given a name. If
          -- so then just use that name.
          names <- gets monomorphedNames
          case Map.lookup fqn names of
            Just name ->
              -- Use the already monomorphed name.
              return (Just name)
            Nothing -> do
              let encoded = encodeDefinitionName fqn
              -- This is the first time we see this definition and try to monomorph
              -- it. First record its qualified name and the flattened name...
              addMonomorphedName fqn encoded
              -- ... then start monomorphing it. If it is recursive the subsequent calls
              -- to 'monomorphDefinition' will use the name we just recorded.
              mExpr <- monomorph expr
              -- Now we can add it to the map of monomorphed definitions, later to be
              -- included in the 'MonomorphedPackage'.
              addMonomorphed fqn (MonomorphedDefinition si encoded mt mExpr)
              return $ Just encoded

    -- Type definitions are not monomorphed or generated to output code, so ignore.
    TypeDefinition{} -> return Nothing

    -- Foreign definitions are are already monomorphic and not generated to out
    -- code, so ignore.
    ForeignDefinition{} -> return Nothing
    
    -- Protocol definitions are not monomorphed or generated to output code, so
    -- ignore.
    ProtocolDefinition{} -> return Nothing
      
    -- Implementations are not monomorphed or generated to output code, so
    -- ignore.
    Implementation{} -> return Nothing


selectForeignImports :: [ImportedPackage TypedPackage]
                     -> [Monomorphed.ForeignPackageImport]
selectForeignImports =
  concatMap toMonomorphedImport
  where
    toMonomorphedImport (ImportedPackage ref name _) =
      case ref of
        ImportReference{} -> []
        ImportForeignReference sourceInfo pkgPath ->
          [Monomorphed.ForeignPackageImport sourceInfo name pkgPath]


-- | Monomorphs a package and returns the complete package with instantiated
-- and monomorphed definitions.
monomorphPackage :: CompileEnvironment
                 -> TypedPackage
                 -> Either MonomorphError MonomorphedPackage
monomorphPackage environment (TypedPackage pkgDecl imports definitions) = do
  let st = MonomorphState { instanceNames = Map.empty
                          , instances = []
                          , monomorphedNames = Map.empty
                          , monomorphed = Map.empty
                          }
      action = mapM_ monomorphDefinition definitions
  (_, s, _) <- runExcept $ runRWST action environment st
  let is = Set.fromList (instances s)
      ms = Set.fromList (Map.elems (monomorphed s))
      imports' = selectForeignImports imports
  return (MonomorphedPackage pkgDecl imports' is ms)


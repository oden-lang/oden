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
import qualified Oden.Core                 as Core
import           Oden.Identifier
import           Oden.Environment                as Environment
import           Oden.Metadata
import           Oden.Predefined
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic     as Mono
import qualified Oden.Type.Polymorphic     as Poly

data MonomorphedDefinition = MonomorphedDefinition (Metadata SourceInfo) Identifier Mono.Type (Core.Expr Mono.Type)
                           deriving (Show, Eq, Ord)

data InstantiatedDefinition =
  InstantiatedDefinition Identifier (Metadata SourceInfo) Identifier (Core.Expr Mono.Type)
  deriving (Show, Eq, Ord)

data MonomorphedPackage = MonomorphedPackage Core.PackageDeclaration
                                             [Core.ImportedPackage]
                                             (Set InstantiatedDefinition)
                                             (Set MonomorphedDefinition)
                     deriving (Show, Eq, Ord)

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
data LetInstance = LetInstance (Metadata SourceInfo) Core.NameBinding (Core.Expr Mono.Type)

data MonomorphState = MonomorphState { instances   :: Map (Identifier, Mono.Type) InstantiatedDefinition
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

addInstance :: (Identifier, Mono.Type) -> InstantiatedDefinition -> Monomorph ()
addInstance key inst =
  modify (\s -> s { instances = Map.insert key inst (instances s) })

addMonomorphed :: Identifier -> MonomorphedDefinition -> Monomorph ()
addMonomorphed identifier def =
  modify (\s -> s { monomorphed = Map.insert identifier def (monomorphed s) })

instantiateDefinition :: (Identifier, Mono.Type)
                      -> Poly.Scheme
                      -> Core.Expr Poly.Type
                      -> Monomorph Identifier
instantiateDefinition key@(pn, t) _ pe = do
  let identifier = Identifier (encodeTypeInstance pn t)
  case instantiate pe t of
    Left err -> throwError (MonomorphInstantiateError err)
    Right expr -> do
      me <- monomorph expr
      addInstance key (InstantiatedDefinition pn (Metadata $ getSourceInfo t) identifier me)
      return identifier

getMonomorphicIn :: CompileEnvironment -> Identifier -> Mono.Type -> Monomorph Identifier
getMonomorphicIn env ident t = do
  def <- lookupIn env ident
  case def of
    Package{} -> throwError $ NotInScope ident
    Definition Core.ForeignDefinition{} -> return ident
    Definition (Core.Definition _ _ (sc, pe)) | Poly.isPolymorphic sc -> do
      let key = (ident, t)
      is <- gets instances
      case Map.lookup key is of
        Just (InstantiatedDefinition _ _ identifier _) -> return identifier
        Nothing -> instantiateDefinition key sc pe
    Definition (Core.Definition _ pn _) -> return pn
    -- Types cannot be referred to at this stage.
    Definition Core.TypeDefinition{} -> throwError $ NotInScope ident
    LetBinding (Core.NameBinding _ boundIdentifier) expr ->
      getMonomorphicLetBinding boundIdentifier t (Core.typeOf expr)
    FunctionArgument (Core.NameBinding _ boundIdentifier) ->
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

getMonoType :: Core.Expr Poly.Type -> Monomorph Mono.Type
getMonoType e = toMonomorphic (Metadata $ getSourceInfo e) (Core.typeOf e)

-- | Return a monomorphic version of a polymorphic expression.
monomorph :: Core.Expr Poly.Type -> Monomorph (Core.Expr Mono.Type)
monomorph e = case e of
  Core.Symbol si ident _ -> do
    env <- ask
    mt <- getMonoType e
    m <- getMonomorphicIn env ident mt
    return (Core.Symbol si m mt)

  Core.Subscript sourceInfo sliceExpr indexExpr polyType ->
    Core.Subscript sourceInfo <$> monomorph sliceExpr
                              <*> monomorph indexExpr
                              <*> toMonomorphic sourceInfo polyType

  Core.Subslice sourceInfo sliceExpr (Core.Range lowerExpr upperExpr) polyType ->
    Core.Subslice sourceInfo <$> monomorph sliceExpr
                             <*> (Core.Range <$> monomorph lowerExpr <*> monomorph upperExpr)
                             <*> toMonomorphic sourceInfo polyType

  Core.Subslice sourceInfo sliceExpr (Core.RangeTo indexExpr) polyType ->
    Core.Subslice sourceInfo <$> monomorph sliceExpr
                             <*> (Core.RangeTo <$> monomorph indexExpr)
                             <*> toMonomorphic sourceInfo polyType

  Core.Subslice sourceInfo sliceExpr (Core.RangeFrom indexExpr) polyType ->
    Core.Subslice sourceInfo <$> monomorph sliceExpr
                             <*> (Core.RangeFrom <$> monomorph indexExpr)
                             <*> toMonomorphic sourceInfo polyType

  Core.UnaryOp si o e1 _ -> do
    mt <- getMonoType e
    me <- monomorph e1
    return (Core.UnaryOp si o me mt)

  Core.BinaryOp si o e1 e2 _ -> do
    mt <- getMonoType e
    me1 <- monomorph e1
    me2 <- monomorph e2
    return (Core.BinaryOp si o me1 me2 mt)

  Core.Application si f p _ -> do
    mt <- getMonoType e
    mf <- monomorph f
    mp <- monomorph p
    return (Core.Application si mf mp mt)

  Core.NoArgApplication si f _ -> do
    mt <- getMonoType e
    mf <- monomorph f
    return (Core.NoArgApplication si mf mt)

  Core.ForeignFnApplication si f ps _ -> do
    mt <- getMonoType e
    mf <- monomorph f
    mps <- mapM monomorph ps
    return (Core.ForeignFnApplication si mf mps mt)

  Core.Fn si param@(Core.NameBinding _ identifier) b _ -> do
    mt <- getMonoType e
    mb <- withIdentifier identifier (FunctionArgument param) (monomorph b)
    return (Core.Fn si param mb mt)

  Core.NoArgFn si b _ -> do
    mt <- getMonoType e
    mb <- monomorph b
    return (Core.NoArgFn si mb mt)

  Core.Slice si es _ -> do
    mt <- getMonoType e
    mes <- mapM monomorph es
    return (Core.Slice si mes mt)

  Core.Block si es _ -> do
    mt <- getMonoType e
    mes <- mapM monomorph es
    return (Core.Block si mes mt)

  Core.Literal si l _ -> do
    mt <- getMonoType e
    return (Core.Literal si l mt)

  Core.Tuple si f s r _ -> do
    mt <- getMonoType e
    mf <- monomorph f
    ms <- monomorph s
    mr <- mapM monomorph r
    return (Core.Tuple si mf ms mr mt)

  Core.If si cond then' else' _ -> do
    mt <- getMonoType e
    mCond <- monomorph cond
    mThen <- monomorph then'
    mElse <- monomorph else'
    return (Core.If si mCond mThen mElse mt)

  Core.Let si b@(Core.NameBinding bsi identifier) expr body _ -> do
    (mBody, allRefs) <- listen (withIdentifier identifier (LetBinding b expr) (monomorph body))
    let (refs, other) = Map.partition (isReferenceTo b) allRefs
    tell other
    insts <- mapM (monomorphReference expr si bsi) (Map.elems refs)
    return (unwrapLetInstances insts mBody)
    where
    isReferenceTo :: Core.NameBinding -> LetReference -> Bool
    isReferenceTo (Core.NameBinding _ identifier') (LetReference letted _ _) =
      identifier' == letted

  Core.RecordInitializer si _ fields -> do
    monoType <- getMonoType e
    monoFields <- mapM monomorphField fields
    return (Core.RecordInitializer si monoType monoFields)
    where
    monomorphField (Core.FieldInitializer fsi label expr) =
      Core.FieldInitializer fsi label <$> monomorph expr

  Core.RecordFieldAccess si expr name _ -> do
    monoType <- getMonoType e
    monoExpr <- monomorph expr
    return (Core.RecordFieldAccess si monoExpr name monoType)

  Core.PackageMemberAccess si pkgAlias name polyType -> do
    env <- ask
    monoType <- toMonomorphic si polyType
    binding <- lookupIn env pkgAlias
    case binding of
      Package _ _ pkgEnv -> do
        m <- getMonomorphicIn pkgEnv name monoType
        return (Core.PackageMemberAccess si pkgAlias m monoType)
      _ -> error "cannot access member in non-existing package"

-- Given a let-bound expression and a reference to that binding, create a
-- monomorphic instance of the let-bound expression.
monomorphReference :: Core.Expr Poly.Type
                   -> Metadata SourceInfo -- Let expression source info.
                   -> Metadata SourceInfo -- Let binding source info.
                   -> LetReference
                   -> Monomorph LetInstance

-- The let-bound value is monomorphic and does not need to be instantiated.
monomorphReference e letSourceInfo bindingSourceInfo (LetReference identifier _ _)
  | not (Poly.isPolymorphicType (Core.typeOf e)) = do
    me <- monomorph e
    return (LetInstance letSourceInfo (Core.NameBinding bindingSourceInfo identifier) me)

-- The let-bound value is polymorphic and must be instantiated.
monomorphReference e letSourceInfo bindingSourceInfo (LetReference _ mt mn) =
  case instantiate e mt of
    Left err -> throwError (MonomorphInstantiateError err)
    Right expr -> do
      me <- monomorph expr
      return (LetInstance letSourceInfo (Core.NameBinding bindingSourceInfo mn) me)

-- | Creates nested let expressions for each let instance around the body
-- expression.
unwrapLetInstances :: [LetInstance]
                   -> Core.Expr Mono.Type -- Body expression.
                   -> Core.Expr Mono.Type
unwrapLetInstances [] body = body
unwrapLetInstances (LetInstance si mn me:is) body =
  Core.Let si mn me (unwrapLetInstances is body) (Core.typeOf body)

-- | Monomorphs a definitions and keeps results in the state.
monomorphDefinitions :: [Core.Definition]
                     -> Monomorph ()
monomorphDefinitions [] = return ()
monomorphDefinitions (d@(Core.Definition si identifier (Poly.Forall _ _ st, expr)) : defs) = do
  case Poly.toMonomorphic st of
    Left _ -> return ()
    Right mt -> do
      mExpr <- monomorph expr
      addMonomorphed identifier (MonomorphedDefinition si identifier mt mExpr)
  -- Monomorph rest of the definitions with this definitions identifier in
  -- the environment.
  local (`extend` (identifier, Definition d)) (monomorphDefinitions defs)
-- Type definitions are not monomorphed or generated to output code, so ignore.
monomorphDefinitions (Core.TypeDefinition{} : defs) =
  monomorphDefinitions defs
-- Foreign definitions are are already monomorphic and not generated to out
-- code, so ignore.
monomorphDefinitions (Core.ForeignDefinition{} : defs) =
  monomorphDefinitions defs

-- | Monomorphs a package and returns the complete package with instantiated
-- and monomorphed definitions.
monomorphPackage :: Core.Package -> Either MonomorphError MonomorphedPackage
monomorphPackage self@(Core.Package pkgDecl imports definitions) = do
  let environment = fromPackage universe `merge` fromPackage self `merge` fromPackages imports
  let st = MonomorphState { instances = Map.empty
                          , monomorphed = Map.empty
                          }
  (_, s, _) <- runExcept $ runRWST (monomorphDefinitions definitions) environment st
  let is = Set.fromList (Map.elems (instances s))
      ms = Set.fromList (Map.elems (monomorphed s))
  return (MonomorphedPackage pkgDecl imports is ms)

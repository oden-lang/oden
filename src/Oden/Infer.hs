{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Oden.Infer (
  TypeError(..),
  Subst(..),
  TypeBinding(..),
  TypingEnvironment,
  inferExpr,
  inferDefinition,
  inferPackage,
  constraintsExpr
) where

import           Control.Arrow                    (left)
import           Control.Monad.Except
import           Control.Monad.RWS                hiding ((<>))

import           Data.Foldable
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Typed                  as Typed
import           Oden.Core.Untyped                hiding (Definition (..),
                                                   MethodImplementation (..))
import qualified Oden.Core.Untyped                as Untyped

import           Oden.Environment                 as Environment hiding (map)
import           Oden.Identifier
import           Oden.Substitution          as Substitution
import           Oden.Metadata
import           Oden.Predefined
import           Oden.QualifiedName               (QualifiedName (..), nameInUniverse)
import           Oden.SourceInfo

import           Oden.Infer.ConstraintCollection
import           Oden.Infer.Environment
import           Oden.Infer.Subsumption
import           Oden.Infer.Unification

import           Oden.Type.Polymorphic
import           Oden.Type.Signature

-- | Inference monad.
type Infer a = (RWST
                  TypingEnvironment -- Typing environment
                  [UnifyConstraint] -- Generated constraints
                  InferState        -- Inference state
                  (Except           -- Inference errors
                    TypeError)
                  a)                -- Result

-- | Inference state.
data InferState = InferState { count :: Int }

-- | Initial inference state.
initInfer :: InferState
initInfer = InferState { count = 0 }

instance FTV TypeBinding where
  ftv (PackageBinding _ _ e)         = ftv e
  ftv (Local _ _ d)           = ftv d
  ftv (Type _ _ _ fs)         = ftv fs
  ftv (QuantifiedType _ _ t)  = ftv t
  ftv (ProtocolBinding _ _ p) = ftv p

instance Substitutable TypeBinding where
  apply _ (PackageBinding si n e)  = PackageBinding si n e
  apply s (Local si n d)           = Local si n (apply s d)
  apply s (Type si n bs t)         = Type si n bs (apply s t)
  apply s (QuantifiedType si n t)  = QuantifiedType si n (apply s t)
  apply s (ProtocolBinding si n p) = ProtocolBinding si n (apply s p)

instance FTV TypingEnvironment where
  ftv (Environment env impls) = ftv (Map.elems env) `Set.union` ftv impls

instance Substitutable TypingEnvironment where
  apply s (Environment env impls) =
    Environment (Map.map (apply s) env) (apply s impls)

data TypeError
  = UnificationError UnificationError
  | PackageNotInScope SourceInfo Identifier
  | NotInScope SourceInfo Identifier
  | MemberNotInPackage SourceInfo Identifier Identifier
  | ArgumentCountMismatch Typed.TypedExpr [Type] [Type]
  | TypeSignatureSubsumptionError Identifier SubsumptionError
  | InvalidPackageReference SourceInfo Identifier
  | ValueUsedAsType SourceInfo Identifier
  | ProtocolUsedAsType SourceInfo Identifier
  | NotAnExpression SourceInfo Identifier
  | NotAProtocol SourceInfo Identifier
  | NoSuchMethodInProtocol SourceInfo Protocol Identifier
  | InvalidForeignFnApplication SourceInfo
  | InvalidForeignExpression SourceInfo
  | TypeAlreadyBound SourceInfo Identifier
  | InvalidImplementationHead SourceInfo TypeSignature
  deriving (Show, Eq)

-- | Run the inference monad.
runInfer :: TypingEnvironment -> Infer a -> Either TypeError (a, [UnifyConstraint])
runInfer env m = runExcept $ evalRWST m env initInfer

-- | Solve for the top-level type of an expression in a given typing
-- environment.
inferExpr :: TypingEnvironment
          -> UntypedExpr
          -> Either TypeError Typed.CanonicalExpr
inferExpr env ex = do
  (te, cs) <- runInfer env (infer ex)
  subst <- left UnificationError $ runSolve cs
  return $ closeOver (apply subst te)

-- | Return the internal constraints used in solving for the type of an
-- expression.
constraintsExpr :: TypingEnvironment
                -> UntypedExpr
                -> Either TypeError ([UnifyConstraint], Subst, Typed.TypedExpr, Scheme)
constraintsExpr env ex = do
  (te, cs) <- runInfer env (infer ex)
  subst <- left UnificationError $ runSolve cs
  let (sc, te') = closeOver $ apply subst te
  return (cs, subst, te', sc)

-- | Canonicalize and return the polymorphic top-level type.
closeOver :: Typed.TypedExpr -> Typed.CanonicalExpr
closeOver = normalize . generalize empty

-- | Unify two types.
uni :: SourceInfo -> Type -> Type -> Infer ()
uni si t1 t2 = tell [UnifyConstraint si t1 t2]

-- | Extend the typing environment.
inEnv :: (Identifier, TypeBinding) -> Infer a -> Infer a
inEnv (x, sc) = local (`extend` (x, sc))

lookupTypeIn :: TypingEnvironment -> Metadata SourceInfo -> Identifier -> Infer Type
lookupTypeIn env (Metadata si) identifier =
  case Environment.lookup identifier env of
    Nothing                     -> throwError $ NotInScope si identifier
    Just PackageBinding{}       -> throwError $ InvalidPackageReference si identifier
    Just Local{}                -> throwError $ ValueUsedAsType si identifier
    Just ProtocolBinding{}      -> throwError $ ProtocolUsedAsType si identifier
    Just (Type _ _ _ t)         -> return t
    Just (QuantifiedType _ _ t) -> return t

-- | Lookup a type in the environment.
lookupType :: Metadata SourceInfo -> Identifier -> Infer Type
lookupType si identifier = do
  env <- ask
  lookupTypeIn env si identifier

lookupValueIn :: TypingEnvironment -> Metadata SourceInfo -> Identifier -> Infer Type
lookupValueIn env (Metadata si) identifier = do
  type' <- case Environment.lookup identifier env of
            Nothing                -> throwError $ NotInScope si identifier
            Just PackageBinding{}  -> throwError $ InvalidPackageReference si identifier
            Just (Local _ _ sc)    -> instantiate si sc
            Just Type{}            -> throwError (NotAnExpression si identifier)
            Just QuantifiedType{}  -> throwError (NotAnExpression si identifier)
            Just ProtocolBinding{} -> throwError (NotAnExpression si identifier)
  return $ setSourceInfo si type'

-- | Lookup type of a value in the environment.
lookupValue :: Metadata SourceInfo -> Identifier -> Infer Type
lookupValue si identifier = do
  env <- ask
  lookupValueIn env si identifier

lookupProtocolIn :: TypingEnvironment
                 -> Metadata SourceInfo
                 -> Identifier
                 -> Infer Protocol
lookupProtocolIn env (Metadata si) identifier =
  case Environment.lookup identifier env of
    Nothing                             -> throwError $ NotInScope si identifier
    Just (ProtocolBinding _ _ protocol) -> return (setSourceInfo si protocol)
    Just _                              -> throwError $ NotAProtocol si identifier

-- | Lookup a protocol in the environment.
lookupProtocol :: Metadata SourceInfo -> Identifier -> Infer Protocol
lookupProtocol si identifier = do
  env <- ask
  lookupProtocolIn env si identifier

findMethod :: SourceInfo -> Protocol -> Identifier -> Infer ProtocolMethod
findMethod si protocol@(Protocol _ _ _ methods) name =
  case find matchesName methods of
    Nothing -> throwError (NoSuchMethodInProtocol si protocol name)
    Just method -> return method
  where
  matchesName (ProtocolMethod _ n _) = n == name

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- | Create a new type variable with a unique name.
fresh :: Metadata SourceInfo -> Infer Type
fresh si = do
    s <- get
    put s{count = count s + 1}
    return $ TVar si (TV ("_t" ++ show (count s)))

-- | Create a type based on a scheme but with all fresh type variables.
instantiate :: SourceInfo -> Scheme -> Infer Type
instantiate si (Forall _ qs cs t) = do
  subst <- Substitution.fromList <$> mapM freshType qs
  let withConstraints = if Set.null cs
                        then t
                        else TConstrained (Set.map setConstraintSourceInfo cs) t
  return (apply subst withConstraints)
  where
  freshType (TVarBinding si' var) =
    (var,) <$> fresh si'
  setConstraintSourceInfo (ProtocolConstraint _ protocolName' type') =
    ProtocolConstraint (Metadata si) protocolName' type'

-- | Given a typed expression, return a canonical expression with the free
-- type variables (not present in the environment) declared as type quantifiers
-- for the expression.
generalize :: TypingEnvironment -> Typed.TypedExpr -> Typed.CanonicalExpr
generalize env expr = (scheme, expr)
  where quantifiers = map (TVarBinding $ Metadata Missing) (Set.toList $ ftv expr `Set.difference` ftv env)
        scheme = Forall (Metadata $ getSourceInfo expr) quantifiers (collectConstraints expr) (typeOf expr)

instantiateMethod :: Metadata SourceInfo
                  -> Protocol
                  -> ProtocolMethod
                  -> Infer (TypedMethodReference, Type)
instantiateMethod constraintSi (Protocol _ protocolName' param _) (ProtocolMethod _ methodName (Forall _ qs cs methodType)) = do
  headTypeVariables <- mapM (freshTypeForFreeVar (getSourceInfo param)) (Set.toList (ftv param))
  subst <- Substitution.fromList <$> ((headTypeVariables ++) <$> mapM freshTypeForBinding qs)
  let constraint = apply subst (ProtocolConstraint constraintSi protocolName' param)
      constrainedType = TConstrained (Set.insert constraint cs) (apply subst methodType)
  return (Typed.Unresolved protocolName' methodName constraint, constrainedType)
  where
  freshTypeForFreeVar si var = (var,) <$> fresh (Metadata si)
  freshTypeForBinding (TVarBinding si var) =
    (var,) <$> fresh si

universeType :: Metadata SourceInfo -> String -> Type
universeType si n = TCon si (nameInUniverse n)

wrapForeign :: Metadata SourceInfo -> Typed.TypedExpr -> Type -> Infer Typed.TypedExpr
wrapForeign si expr t =
  case t of
    TForeignFn _ _ [] returnTypes -> do
      let wrappedReturnType = wrapReturnTypes returnTypes
          innerExpr = ForeignFnApplication si expr [] wrappedReturnType
      return $ NoArgFn si innerExpr (TNoArgFn si wrappedReturnType)
    TForeignFn _ _ parameterTypes returnTypes -> do
      let generatedNames = map (Identifier . ("_g" ++) . show) [(0 :: Int)..]
          namesAndTypes = zip generatedNames parameterTypes
          wrappedReturnType = wrapReturnTypes returnTypes
          innerExpr = ForeignFnApplication si expr (map (uncurry $ Symbol si) namesAndTypes) wrappedReturnType
      fst <$> foldM wrapInFn (innerExpr, wrappedReturnType) (reverse namesAndTypes)
    _ -> return expr
  where
  wrapInFn (expr', returnType) (name, type') =
    let fnType = TFn si type' returnType
    in return (Fn si (NameBinding si name) expr' fnType, fnType)
  wrapReturnTypes [] = TCon si (nameInUniverse "unit")
  wrapReturnTypes [returnType] = returnType
  wrapReturnTypes (ft:st:ts) = TTuple si ft st ts

-- | The heart of the inferencer. Takes an untyped expression and returns the
-- inferred typed expression. Constraints are collected in the 'Infer' monad
-- and substitutions are made before the inference is complete, so the
-- expressions returned from 'infer' are not the final results.
infer :: UntypedExpr -> Infer Typed.TypedExpr
infer = \case
  Literal si Unit Untyped ->
    return (Literal si Unit (TCon si (nameInUniverse "unit")))
  Literal si (Int n) Untyped ->
    return (Literal si (Int n) (TCon si (nameInUniverse "int")))
  Literal si (Float n) Untyped ->
    return (Literal si (Float n) (TCon si (nameInUniverse "float64")))
  Literal si (Bool b) Untyped ->
    return (Literal si (Bool b) (TCon si (nameInUniverse "bool")))
  Literal si (String s) Untyped ->
    return (Literal si (String s) (TCon si (nameInUniverse "string")))

  Subscript si s i Untyped -> do
    st <- infer s
    it <- infer i
    tv <- fresh si
    uni (getSourceInfo st) (typeOf st) (TSlice (Metadata $ getSourceInfo s) tv)
    uni (getSourceInfo it) (typeOf it) (TCon si (nameInUniverse "int"))
    return (Subscript si st it tv)

  Subslice si s range Untyped ->
    case range of
      (Range lowerExpr upperExpr) -> do
        st <- infer s
        lowerExprTyped <- infer lowerExpr
        upperExprTyped <- infer upperExpr
        tv <- fresh si
        uni (getSourceInfo st) (typeOf st) (TSlice (Metadata $ getSourceInfo s) tv)
        uni (getSourceInfo upperExprTyped) (typeOf lowerExprTyped) (universeType si "int")
        uni (getSourceInfo upperExprTyped) (typeOf upperExprTyped) (universeType si "int")
        return (Subslice si st (Range lowerExprTyped upperExprTyped) (TSlice si tv))
      (RangeTo upperExpr) -> inferUnboundedRange RangeTo upperExpr
      (RangeFrom lowerExpr) -> inferUnboundedRange RangeFrom lowerExpr

    where
    inferUnboundedRange f boundExpr = do
      st <- infer s
      boundExprTyped <- infer boundExpr
      tv <- fresh si
      uni (getSourceInfo st) (typeOf st) (TSlice (Metadata $ getSourceInfo s) tv)
      uni (getSourceInfo boundExprTyped) (typeOf boundExprTyped) (universeType si "int")
      return (Subslice si st (f boundExprTyped) (TSlice si tv))

  Symbol si x Untyped -> do
    t <- lookupValue si x
    wrapForeign si (Symbol si x t) t

  MemberAccess si (NamedMemberAccess expr'@(Symbol symbolSourceInfo name Untyped) memberName) Untyped -> do
    env <- ask
    case Environment.lookup name env of
      Just (PackageBinding _ _ pkgEnv) -> do
        valueType <- lookupValueIn pkgEnv si memberName
        wrapForeign si (MemberAccess si (PackageMemberAccess name memberName) valueType) valueType
      Just _ -> inferRecordFieldAccess si expr' memberName
      Nothing -> throwError $ NotInScope (unwrap symbolSourceInfo) name

  MemberAccess si (NamedMemberAccess expr' fieldName) Untyped ->
    inferRecordFieldAccess si expr' fieldName

  Fn si (NameBinding bsi a) b Untyped -> do
    tv <- fresh bsi
    tb <- inEnv (a, Local bsi a (Forall bsi [] Set.empty tv)) (infer b)
    return (Fn si (NameBinding bsi a) tb (TFn si tv (typeOf tb)))

  NoArgFn si f Untyped -> do
    tf <- infer f
    return (NoArgFn si tf (TNoArgFn si (typeOf tf)))

  NoArgApplication si f Untyped -> do
    tv <- fresh si
    tf <- infer f
    uni (getSourceInfo tf)
      (typeOf tf)
      (TNoArgFn (Metadata $ getSourceInfo tf) tv)
    return (NoArgApplication si tf tv)

  Application si f p Untyped -> do
    tv <- fresh si
    tf <- infer f
    tp <- infer p
    uni (getSourceInfo tf)
        (typeOf tf)
        (TFn (Metadata $ getSourceInfo tf) (typeOf tp) tv)
    return (Application si tf tp tv)

  Let si (NameBinding bsi n) e b Untyped -> do
    te <- infer e
    tb <- inEnv (n, Local bsi n (Forall si [] Set.empty (typeOf te))) (infer b)
    return (Let si (NameBinding bsi n) te tb (typeOf tb))

  If si cond tr fl Untyped -> do
    tcond <- infer cond
    ttr <- infer tr
    tfl <- infer fl
    uni (getSourceInfo tcond) (typeOf tcond) (universeType si "bool")
    uni (getSourceInfo ttr) (typeOf ttr) (typeOf tfl)
    return (If si tcond ttr tfl (typeOf ttr))

  Tuple si f s r Untyped -> do
    tf <- infer f
    ts <- infer s
    tr <- mapM infer r
    let t = TTuple si (typeOf tf) (typeOf ts) (map typeOf tr)
    return (Tuple si tf ts tr t)

  Slice si es Untyped -> do
    tv <- fresh si
    tes <- mapM infer es
    mapM_ (uni (unwrap si) tv . typeOf) tes
    return (Slice si tes (TSlice si tv))

  Block si es Untyped -> do
    tv <- fresh si
    tes <- mapM infer es
    case tes of
      [] -> uni (unwrap si) tv (universeType si "unit")
      _ -> uni (unwrap si) tv (typeOf (last tes))
    return (Block si tes tv)

  RecordInitializer si fields Untyped -> do
    (fieldInitializers, row) <- foldM unifyFields ([], REmpty si) fields
    return (RecordInitializer si fieldInitializers (TRecord si row))
    where
    unifyFields (typedFields, row) (FieldInitializer fsi label expr') = do
      typedExpr <- infer expr'
      return (FieldInitializer fsi label typedExpr : typedFields, RExtension fsi label (typeOf typedExpr) row)

  MethodReference si (NamedMethodReference protocol method) Untyped -> do
    protocolType' <- lookupProtocol si protocol
    method' <- findMethod (unwrap si) protocolType' method
    (ref, methodType) <- instantiateMethod si protocolType' method'
    return (MethodReference si ref methodType)

  ForeignFnApplication (Metadata si) _ _ _ ->
    -- Untyped code is not allowed to use foreign function application
    -- directly, that is only used by the compiler after type inference.
    throwError (InvalidForeignFnApplication si)

  Foreign (Metadata si) _ _ ->
    -- Untyped code is not allowed to use foreign expressions directly, that is
    -- only used by the compiler after type inference.
    throwError (InvalidForeignExpression si)

  where
  inferRecordFieldAccess si expr' label = do
    fieldType <- fresh si
    recordExtType <- fresh si
    typedExpr <- infer expr'
    uni (getSourceInfo typedExpr)
        (TRecord (Metadata $ getSourceInfo typedExpr) (RExtension si label fieldType recordExtType))
        (typeOf typedExpr)
    return (MemberAccess si (RecordFieldAccess typedExpr label) fieldType)

-- | Tries to resolve a user-supplied type expression to an actual type.
resolveType :: SignatureExpr -> Infer Type
resolveType =
  \case
    TSUnit si ->
      return (universeType (Metadata si) "unit")
    TSSymbol si i ->
      lookupType (Metadata si) i
    TSApp si cons param ->
      TApp (Metadata si) <$> resolveType cons <*> resolveType param
    TSFn si de re ->
      TFn (Metadata si) <$> resolveType de <*> resolveType re
    TSNoArgFn si e ->
      TNoArgFn (Metadata si) <$> resolveType e
    TSTuple si fe se re ->
      TTuple (Metadata si) <$> resolveType fe
                           <*> resolveType se
                           <*> mapM resolveType re
    TSSlice si e ->
      TSlice (Metadata si) <$> resolveType e
    TSRowEmpty si ->
      return (REmpty (Metadata si))
    TSRowExtension si label type' row ->
      RExtension (Metadata si) label <$> resolveType type'
                                    <*> resolveType row
    TSRecord si r ->
      TRecord (Metadata si) <$> resolveType r

extendEnvWithBindings :: [SignatureVarBinding] -> TypingEnvironment -> Infer TypingEnvironment
extendEnvWithBindings bindings' env =
  foldM extendWithBinding env bindings'
  where
  extendWithBinding env' (SignatureVarBinding si' v) = do
    tv <- fresh (Metadata si')
    return $ env' `extend` (v, QuantifiedType (Metadata si') v tv)


-- | Tries to resolve a user-supplied type signature to an actual type scheme.
resolveTypeSignature :: TypeSignature -> Infer (Scheme, TypingEnvironment)
resolveTypeSignature (TypeSignature si bindings' expr) = do
  env <- ask >>= extendEnvWithBindings bindings'
  t <- local (const env) (resolveType expr)
  -- NOTE: Type constraints in signature not supported yet, always an empty
  -- list.
  return (Forall (Metadata si) (map toVarBinding bindings') Set.empty t, env)
  where
  toVarBinding (SignatureVarBinding si' (Identifier v)) = TVarBinding (Metadata si') (TV v)

resolveMethod :: ProtocolMethodSignature -> Infer ProtocolMethod
resolveMethod (ProtocolMethodSignature si name signature) = do
  (scheme, _) <- resolveTypeSignature signature
  return (ProtocolMethod (Metadata si) name scheme)

inferMethodImplementation :: Protocol
                          -> Untyped.MethodImplementation
                          -> Infer (MethodImplementation TypedExpr)
inferMethodImplementation protocol (Untyped.MethodImplementation si name expr) = do
  ProtocolMethod _ methodName methodScheme <- findMethod (unwrap si) protocol name
  let (Forall _ _ _ methodType) = methodScheme
  typedExpr <- infer expr
  uni (getSourceInfo typedExpr) (typeOf typedExpr) methodType
  return (MethodImplementation si methodName typedExpr)


-- | Indicates if the canonical expression should be generated by closing over
-- the the free type variables in the inferred expression. This is done when
-- top type signatures are missing.
type ShouldCloseOver = Bool


-- | Infer the untyped definition in the Infer monad, returning a typed
-- version. Resolves type signatures of optionally type-annotated definitions.
inferDef :: Untyped.Definition -> Infer (Typed.TypedDefinition, ShouldCloseOver)
inferDef =
  \case
    Untyped.Definition si name signature expr -> do
      env <- ask
      case signature of
        Nothing -> do
          tv <- fresh si
          let recScheme = Forall si [] Set.empty tv
          let recursiveEnv = env `extend` (name, Local si name recScheme)
          te <- local (const recursiveEnv) (infer expr)
          return (Definition si name (recScheme, te), True)
        Just ts -> do
          (recScheme@(Forall _ _ _ recType), envWithBindings) <- resolveTypeSignature ts
          let recursiveEnv = envWithBindings  `extend` (name, Local si name recScheme)
          te <- local (const recursiveEnv) (infer expr)
          uni (getSourceInfo te) (typeOf te) recType
          case recScheme `subsumedBy` te of
            Left e -> throwError $ TypeSignatureSubsumptionError name e
            Right canonical -> return (Definition si name canonical, False)

    Untyped.TypeDefinition si name params typeExpr -> do
      type' <- resolveType typeExpr
      return (TypeDefinition si name (map convertParams params) type', False)
      where
      convertParams (NameBinding bsi bn) = NameBinding bsi bn

    Untyped.ProtocolDefinition si name (SignatureVarBinding vsi var) methods -> do
      validateUnboundType var
      let boundType = TVar (Metadata vsi) (TV (asString var))
      methods' <- local (`extend` (var, QuantifiedType (Metadata vsi) var boundType))
                        (mapM resolveMethod methods)
      let protocol = Protocol si name boundType methods'
      return (ProtocolDefinition si name protocol, False)
      where
      validateUnboundType :: Identifier -> Infer ()
      validateUnboundType identifier = do
        env <- ask
        case Environment.lookup identifier env of
          Just PackageBinding{}  -> throwError $ InvalidPackageReference vsi identifier
          Just Local{}           -> throwError $ ValueUsedAsType vsi identifier
          Just ProtocolBinding{} -> throwError $ ProtocolUsedAsType vsi identifier
          Just Type{}            -> throwError $ TypeAlreadyBound vsi identifier
          Just QuantifiedType{}  -> throwError $ TypeAlreadyBound vsi identifier
          Nothing                -> return ()

    Untyped.Implementation si signature@(TypeSignature tsi qs headType) methods ->
      case headType of
        TSApp _ (TSSymbol _ protocolName') type' -> do
          protocol <- lookupProtocol si protocolName'
          env <- ask >>= extendEnvWithBindings qs
          resolvedType <- local (const env) (resolveType type')
          uni (unwrap si) (protocolHead protocol) resolvedType
          methodImplementations <- local (const env) (mapM (inferMethodImplementation protocol) methods)
          let impl = ProtocolImplementation si (protocolName protocol) resolvedType methodImplementations
          return (Implementation si impl, False)
        _ -> throwError (InvalidImplementationHead tsi signature)


-- | Infer a top-level definitition, returning a typed version and the typing
-- environment extended with the definition.
inferDefinition :: TypingEnvironment
                -> Untyped.Definition
                -> Either TypeError (TypingEnvironment, Typed.TypedDefinition)
inferDefinition env def = do
  -- Infer the definition.
  ((def', shouldCloseOver), cs) <- runInfer env (inferDef def)

  case def' of
    Definition si name (_, te) | shouldCloseOver -> do
      subst <- left UnificationError $ runSolve cs
      let canonical'@(sc, _) = closeOver (apply subst te)
          env' = env `extend` (name, Local si name sc)
      return (env', Definition si name canonical')
    Definition si name canonical -> do
      subst <- left UnificationError $ runSolve cs
      let canonical'@(sc, _) = normalize (apply subst canonical)
          env' = env `extend` (name, Local si name sc)
      return (env', Definition si name canonical')

    TypeDefinition si name@(FQN _ localName) params type' ->
      return (env `extend` (localName, Type si name params type'), def')

    ProtocolDefinition si (FQN _ localName) protocol ->
      return (env `extend` (localName, ProtocolBinding si localName protocol), def')

    Implementation si impl -> do
      subst <- left UnificationError $ runSolve cs
      let substImpl = apply subst impl
      return (substImpl `addImplementation` env, Implementation si substImpl)

    ForeignDefinition _ name _ ->
      error ("unexpected foreign definition: " ++ asString name)

-- | Infer the package, returning a package with typed definitions along with
-- the extended typing environment.
inferPackage :: UntypedPackage (ImportedPackage TypedPackage)
             -> Either TypeError (TypingEnvironment, TypedPackage)
inferPackage (UntypedPackage (PackageDeclaration psi name) imports defs) = do
  let env = fromPackage universe `merge` fromPackages imports
  (extendedEnv, inferred) <- foldM iter (env, []) defs
  return (extendedEnv,
          TypedPackage (PackageDeclaration psi name) imports inferred)
  where
  iter (e, inferred) def = do
      (e', def') <- inferDefinition e def
      return (e', inferred ++ [def'])

-- | Swaps all type variables names for generated ones based on 'letters' to
-- get a nice sequence.
normalize :: (Scheme, Typed.TypedExpr) -> (Scheme, Typed.TypedExpr)
normalize (Forall si _ constraints exprType, te) =
  (Forall si newBindings (apply subst constraints) (apply subst exprType), apply subst te)
  where
    -- Pairs of existing type variables in the type and new type variables
    -- values to substitute with, a sequence based on 'letters'.
    substPairs = zip (Set.toList $ ftv exprType) (map TV letters)
    -- The substitution based on the pairs.
    subst = Subst (Map.fromList (map wrapTvar substPairs))
    wrapTvar (tv1, tv2) = (tv1, TVar (Metadata Missing) tv2)
    -- The new set of type variables bindings for the canonical expression.
    newBindings = map (TVarBinding (Metadata Missing) . snd) substPairs

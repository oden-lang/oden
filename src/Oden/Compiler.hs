module Oden.Compiler where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Map                  as Map hiding (map)
import           Data.Set                  as Set hiding (map)

import           Oden.Compiler.Instantiate
import           Oden.Compiler.TypeEncoder
import qualified Oden.Core                 as Core
import           Oden.Identifier
import           Oden.Scope                as Scope
import qualified Oden.Type.Monomorphic     as Mono
import qualified Oden.Type.Polymorphic     as Poly

data MonomorphedDefinition = MonomorphedDefinition Name Mono.Type (Core.Expr Mono.Type)
                           deriving (Show, Eq, Ord)

data InstantiatedDefinition =
  InstantiatedDefinition Name (Core.Expr Mono.Type)
  deriving (Show, Eq, Ord)

data CompiledPackage = CompiledPackage Core.PackageName [Core.Import] (Set InstantiatedDefinition) (Set MonomorphedDefinition)
                     deriving (Show, Eq, Ord)

data CompilationError = NotInScope Identifier
                      | AmbigiousReference Identifier [(Scope.Source, Scope.Definition)]
                      | UnexpectedPolyType (Core.Expr Poly.Type)
                      | MonomorphInstantiateError InstantiateError
                      deriving (Show, Eq, Ord)

data LocalBinding = LetBinding Name (Core.Expr Poly.Type)
                  | FunctionArgument Name

type LocalBindings = Map Name LocalBinding

data LetReference = LetReference Name Mono.Type Name
                 deriving (Show, Eq, Ord)

data LetInstance = LetInstance Name (Core.Expr Mono.Type)

data MonomorphState = MonomorphState { instances   :: Map (Identifier, Mono.Type) InstantiatedDefinition
                                     , monomorphed :: Map Identifier MonomorphedDefinition
                                     , scope       :: Scope
                                     }

type Monomorph a = RWST LocalBindings
                        (Set LetReference)
                        MonomorphState
                        (Except CompilationError)
                        a

getInScope :: Identifier -> Monomorph Definition
getInScope i = do
  s <- gets scope
  case Scope.lookup i s of
    [] -> throwError (NotInScope i)
    [(_, d)] -> return d
    found -> throwError (AmbigiousReference i found)

addToScope :: Core.Definition -> Monomorph ()
addToScope (Core.Definition name (sc, expr))= do
  scope' <- gets scope
  modify (\s -> s { scope = Scope.insert Scope.Definitions (Unqualified name) (Scope.OdenDefinition (Unqualified name) sc expr) scope' })

withBinding :: LocalBinding -> Monomorph a -> Monomorph a
withBinding b@(LetBinding name _) = local $ Map.insert name b
withBinding b@(FunctionArgument name) = local $ Map.insert name b

addInstance :: (Identifier, Mono.Type) -> InstantiatedDefinition -> Monomorph ()
addInstance key inst =
  modify (\s -> s { instances = Map.insert key inst (instances s) })

addMonomorphed :: Name -> MonomorphedDefinition -> Monomorph ()
addMonomorphed name def =
  modify (\s -> s { monomorphed = Map.insert (Unqualified name) def (monomorphed s) })

instantiateDefinition :: (Identifier, Mono.Type)
                      -> Poly.Scheme
                      -> Core.Expr Poly.Type
                      -> Monomorph Identifier
instantiateDefinition key@(pn, t) _ pe = do
  let name = encodeTypeInstance pn t
  case instantiate pe t of
    Left err -> throwError (MonomorphInstantiateError err)
    Right expr -> do
      me <- monomorph expr
      addInstance key (InstantiatedDefinition name me)
      return (Unqualified name)

getMonomorphicDefinition :: Identifier -> Mono.Type -> Monomorph Identifier
getMonomorphicDefinition ident t = do
  def <- getInScope ident
  case def of
    ForeignDefinition gn _ -> return gn
    OdenDefinition _ sc pe | Poly.isPolymorphic sc -> do
      let key = (ident, t)
      is <- gets instances
      case Map.lookup key is of
        Just (InstantiatedDefinition name _) -> return (Unqualified name)
        Nothing -> instantiateDefinition key sc pe
    OdenDefinition pn _ _ -> return pn

getMonomorphicLetBinding :: Name
                         -> Mono.Type
                         -> Poly.Type
                         -> Monomorph Identifier
getMonomorphicLetBinding n mt pt | not (Poly.isPolymorphicType pt) = do
  tell (Set.singleton (LetReference n mt n))
  return (Unqualified n)
getMonomorphicLetBinding n mt _ = do
  let encoded = encodeTypeInstance (Unqualified n) mt
  tell (Set.singleton (LetReference n mt encoded))
  return (Unqualified encoded)

getMonomorphic :: Identifier -> Mono.Type -> Monomorph Identifier
getMonomorphic i@(Qualified _ _) t =
  local (const Map.empty) (getMonomorphicDefinition i t)
getMonomorphic i@(Unqualified n) t = do
  binding <- Map.lookup n <$> ask
  case binding of
    Just (FunctionArgument a) ->
      return (Unqualified a)
    Just (LetBinding n' e) ->
      getMonomorphicLetBinding n' t (Core.typeOf e)
    Nothing ->
      local (const Map.empty) (getMonomorphicDefinition i t)

getMonoType :: Core.Expr Poly.Type -> Monomorph Mono.Type
getMonoType e =
  either (const $ throwError (UnexpectedPolyType e))
         return
         (Poly.toMonomorphic (Core.typeOf e))

monomorph :: Core.Expr Poly.Type -> Monomorph (Core.Expr Mono.Type)
monomorph e@(Core.Symbol ident _) = do
  mt <- getMonoType e
  m <- getMonomorphic ident mt
  return (Core.Symbol m mt)
monomorph e@(Core.Op o e1 e2 _) = do
  mt <- getMonoType e
  me1 <- monomorph e1
  me2 <- monomorph e2
  return (Core.Op o me1 me2 mt)
monomorph e@(Core.Application f p _) = do
  mt <- getMonoType e
  mf <- monomorph f
  mp <- monomorph p
  return (Core.Application mf mp mt)
monomorph e@(Core.NoArgApplication f _) = do
  mt <- getMonoType e
  mf <- monomorph f
  return (Core.NoArgApplication mf mt)
monomorph e@(Core.UncurriedFnApplication f ps _) = do
  mt <- getMonoType e
  mf <- monomorph f
  mps <- mapM monomorph ps
  return (Core.UncurriedFnApplication mf mps mt)
monomorph e@(Core.Fn a b _) = do
  mt <- getMonoType e
  mb <- withBinding (FunctionArgument a) (monomorph b)
  return (Core.Fn a mb mt)
monomorph e@(Core.NoArgFn b _) = do
  mt <- getMonoType e
  mb <- monomorph b
  return (Core.NoArgFn mb mt)
monomorph e@(Core.Slice es _) = do
  mt <- getMonoType e
  mes <- mapM monomorph es
  return (Core.Slice mes mt)
monomorph e@(Core.Block es _) = do
  mt <- getMonoType e
  mes <- mapM monomorph es
  return (Core.Block mes mt)
monomorph e@(Core.Literal l _) = do
  mt <- getMonoType e
  return (Core.Literal l mt)
monomorph e@(Core.If cond then' else' _) = do
  mt <- getMonoType e
  mCond <- monomorph cond
  mThen <- monomorph then'
  mElse <- monomorph else'
  return (Core.If mCond mThen mElse mt)
monomorph (Core.Let name expr body _) = do
  (mBody, allRefs) <- listen (withBinding (LetBinding name expr) (monomorph body))
  let (refs, other) = Set.partition (isReferenceTo name) allRefs
  tell other
  insts <- mapM (monomorphReference expr) (Set.toList refs)
  return (unwrapLetInstances insts mBody)
  where
  isReferenceTo :: Name -> LetReference -> Bool
  isReferenceTo n (LetReference ln _ _) = n == ln

monomorphReference :: Core.Expr Poly.Type -> LetReference -> Monomorph LetInstance
monomorphReference e (LetReference n _ _) | not (Poly.isPolymorphicType (Core.typeOf e)) = do
  me <- monomorph e
  return (LetInstance n me)
monomorphReference e (LetReference _ mt mn) =
  case instantiate e mt of
    Left err -> throwError (MonomorphInstantiateError err)
    Right expr -> do
      me <- monomorph expr
      return (LetInstance mn me)

unwrapLetInstances :: [LetInstance] -> Core.Expr Mono.Type -> Core.Expr Mono.Type
unwrapLetInstances [] body = body
unwrapLetInstances (LetInstance mn me:is) body = Core.Let mn me (unwrapLetInstances is body) (Core.typeOf body)

monomorphDefinition :: Core.Definition -> Monomorph ()
monomorphDefinition d@(Core.Definition name (Poly.Forall _ st, expr)) = do
  addToScope d
  case Poly.toMonomorphic st of
    Left _ -> return ()
    Right mt -> do
      mExpr <- monomorph expr
      addMonomorphed name (MonomorphedDefinition name mt mExpr)

monomorphPackage :: Scope -> Core.Package -> Either CompilationError CompiledPackage
monomorphPackage scope' (Core.Package name imports definitions) = do
  let st = MonomorphState { instances = Map.empty
                          , monomorphed = Map.empty
                          , scope = scope'
                          }
  (_, s, _) <- runExcept $ runRWST (mapM_ monomorphDefinition definitions) Map.empty st
  let is = Set.fromList (Map.elems (instances s))
      ms = Set.fromList (Map.elems (monomorphed s))
  return (CompiledPackage name imports is ms)

compile :: Scope -> Core.Package -> Either CompilationError CompiledPackage
compile = monomorphPackage

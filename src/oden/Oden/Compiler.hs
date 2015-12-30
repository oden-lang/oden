module Oden.Compiler where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Map                  as Map hiding (map)
import           Data.Maybe
import           Data.Set                  as Set hiding (map)

import           Oden.Compiler.Instantiate
import           Oden.Compiler.Scope       as Scope
import           Oden.Compiler.TypeEncoder
import qualified Oden.Core                 as Core
import qualified Oden.Env                  as Env
import           Oden.Identifier
import qualified Oden.Type.Monomorphic     as Mono
import qualified Oden.Type.Polymorphic     as Poly

data MonomorphedDefinition = MonomorphedDefinition Name (Core.Expr Mono.Type)
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

data LetInstance = LetInstance (Name, Mono.Type) Name (Core.Expr Mono.Type)
                 deriving (Show, Eq, Ord)

data MonomorphState = MonomorphState { instances   :: Map (Identifier, Mono.Type) InstantiatedDefinition
                                     , monomorphed :: Map Identifier MonomorphedDefinition
                                     , scope       :: Scope
                                     }

type Monomorph a = RWST LocalBindings
                        (Set LetInstance)
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
withBinding b@(LetBinding name ce) = local $ Map.insert name b
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
instantiateDefinition key@(pn, t) sc pe = do
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
    OdenDefinition pn sc pe | Poly.isPolymorphic sc -> do
      let key = (ident, t)
      is <- gets instances
      case Map.lookup key is of
        Just (InstantiatedDefinition name _) -> return (Unqualified name)
        Nothing -> instantiateDefinition key sc pe
    OdenDefinition pn _ _ -> return pn

-- NOTE: This function always performs the work of instantiating the let bound
-- value, even if it has been done before. Maybe move LetInstances to state and
-- keep them in a Map instead to avoid this.
getMonomorphicLetBinding :: (Name, Mono.Type)
                         -> Core.Expr Poly.Type
                         -> Monomorph Identifier
getMonomorphicLetBinding key@(n, t) e | not(Poly.isPolymorphicType (Core.typeOf e)) = do
  me <- monomorph e
  tell (Set.singleton (LetInstance key n me))
  return (Unqualified n)
getMonomorphicLetBinding key@(n, t) e = do
  let name = encodeTypeInstance (Unqualified n) t
  case instantiate e t of
    Left err -> throwError (MonomorphInstantiateError err)
    Right expr -> do
      me <- monomorph expr
      tell (Set.singleton (LetInstance key name me))
      return (Unqualified name)

getMonomorphic :: Identifier -> Mono.Type -> Monomorph Identifier
getMonomorphic i@(Qualified _ _) t =
  local (const Map.empty) (getMonomorphicDefinition i t)
getMonomorphic i@(Unqualified n) t = do
  binding <- Map.lookup n <$> ask
  case binding of
    Just (FunctionArgument a) ->
      return (Unqualified a)
    Just (LetBinding n' e) ->
      getMonomorphicLetBinding (n', t) e
    Nothing ->
      local (const Map.empty) (getMonomorphicDefinition i t)

getMonoType :: Core.Expr Poly.Type -> Monomorph Mono.Type
getMonoType e =
  either (const $ throwError (UnexpectedPolyType e))
         return
         (Poly.toMonomorphic (Core.typeOf e))

unwrapLetInstances :: [LetInstance] -> Core.Expr Mono.Type -> Core.Expr Mono.Type
unwrapLetInstances [] body = body
unwrapLetInstances (LetInstance _ mn me:is) body = Core.Let mn me (unwrapLetInstances is body) (Core.typeOf body)

monomorph :: Core.Expr Poly.Type -> Monomorph (Core.Expr Mono.Type)
monomorph e@(Core.Symbol ident t) = do
  mt <- getMonoType e
  m <- getMonomorphic ident mt
  return (Core.Symbol m mt)
monomorph e@(Core.Application f p t) = do
  mt <- getMonoType e
  mf <- monomorph f
  mp <- monomorph p
  return (Core.Application mf mp mt)
monomorph e@(Core.NoArgApplication f t) = do
  mt <- getMonoType e
  mf <- monomorph f
  return (Core.NoArgApplication mf mt)
monomorph e@(Core.Fn a b t) = do
  mt <- getMonoType e
  mb <- withBinding (FunctionArgument a) (monomorph b)
  return (Core.Fn a mb mt)
monomorph e@(Core.NoArgFn b t) = do
  mt <- getMonoType e
  mb <- monomorph b
  return (Core.NoArgFn mb mt)
monomorph e@(Core.Literal l t) = do
  mt <- getMonoType e
  return (Core.Literal l mt)
monomorph e@(Core.If cond then' else' t) = do
  mt <- getMonoType e
  mCond <- monomorph cond
  mThen <- monomorph then'
  mElse <- monomorph else'
  return (Core.If mCond mThen mElse mt)
monomorph e@(Core.Let name expr body t) = do
  mt <- getMonoType e
  (mBody, allInstances) <- listen (withBinding (LetBinding name expr) (monomorph body))
  let (letInstances, other) = Set.partition (isLetInstanceFrom name) allInstances
  tell other
  let lExpr = unwrapLetInstances (Set.toList letInstances) mBody
  return lExpr
  where
  isLetInstanceFrom :: Name -> LetInstance -> Bool
  isLetInstanceFrom n (LetInstance (ln, _) _ _) = n == ln

monomorphDefinition :: Core.Definition -> Monomorph ()
monomorphDefinition d@(Core.Definition name (s, expr)) = do
  addToScope d
  unless (Poly.isPolymorphic s) $ do
    mExpr <- monomorph expr
    addMonomorphed name (MonomorphedDefinition name mExpr)

monomorphPackage :: Env.Env -> Core.Package -> Either CompilationError CompiledPackage
monomorphPackage predefined (Core.Package name imports definitions) = do
  let st = MonomorphState { instances = Map.empty
                          , monomorphed = Map.empty
                          , scope = predefinedEnvToScope predefined
                          }
  (_, s, _) <- runExcept $ runRWST (mapM_ monomorphDefinition definitions) Map.empty st
  let is = Set.fromList (Map.elems (instances s))
      ms = Set.fromList (Map.elems (monomorphed s))
  return (CompiledPackage name imports is ms)

compile :: Env.Env -> Core.Package -> Either CompilationError CompiledPackage
compile = monomorphPackage

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

data PolymorphicDefinition =
  PolymorphicDefinition Name (Core.Expr Poly.Type)
  deriving (Show, Eq, Ord)

data InstantiatedDefinition =
  InstantiatedDefinition Name (Core.Expr Mono.Type)
  deriving (Show, Eq, Ord)

data CompiledPackage = CompiledPackage Core.PackageName [Core.Import] (Set InstantiatedDefinition) (Set MonomorphedDefinition)
                     deriving (Show, Eq, Ord)

data CompilationError = NotInScope Identifier
                      | AmbigiousReference Identifier [(Scope.Source, Scope.Definition)]
                      | UnexpectedPolyType Poly.Type
                      | MonomorphInstantiateError InstantiateError
                      deriving (Show, Eq, Ord)

data MonomorphLocals = MonomorphLocals (Set Name)

data MonomorphState = MonomorphState { instances   :: Map (Identifier, Mono.Type) InstantiatedDefinition
                                     , monomorphed :: Map Identifier MonomorphedDefinition
                                     , scope       :: Scope
                                     }

type Monomorph a = RWST MonomorphLocals () MonomorphState (Except CompilationError) a

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

withBinding :: Name -> Monomorph a -> Monomorph a
withBinding n = local (\(MonomorphLocals locals) -> MonomorphLocals (Set.insert n locals))

addInstance :: (Identifier, Mono.Type) -> InstantiatedDefinition -> Monomorph ()
addInstance key inst =
  modify (\s -> s { instances = Map.insert key inst (instances s) })

addMonomorphed :: Name -> MonomorphedDefinition -> Monomorph ()
addMonomorphed name def =
  modify (\s -> s { monomorphed = Map.insert (Unqualified name) def (monomorphed s) })

getInstantiated :: Identifier -> Mono.Type -> Monomorph Identifier
getInstantiated ident t = do
  def <- getInScope ident
  case def of
    ForeignDefinition gn _ -> return gn
    OdenDefinition pn sc pe | Poly.isPolymorphic sc -> do
      let key = (ident, t)
      is <- gets instances
      case Map.lookup key is of
        Just (InstantiatedDefinition name _) -> return (Unqualified name)
        Nothing -> do
          let name = encodeTypeInstance pn t
          case instantiate pe t of
            Left err -> throwError (MonomorphInstantiateError err)
            Right expr -> do
              me <- monomorph expr
              addInstance key (InstantiatedDefinition name me)
              return (Unqualified name)
    OdenDefinition pn _ _ -> return pn

polyToMono :: Poly.Type -> Monomorph Mono.Type
polyToMono t = either (const $ throwError (UnexpectedPolyType t)) return (Poly.toMonomorphic t)

isLocal :: Identifier -> Monomorph Bool
isLocal (Unqualified n) = do
  MonomorphLocals bindings <- ask
  return (n `elem` bindings)
isLocal _ = return False

monomorph :: Core.Expr Poly.Type -> Monomorph (Core.Expr Mono.Type)
monomorph (Core.Symbol ident t) = do
  mt <- polyToMono t
  isLocal' <- isLocal ident
  if isLocal'
  then return (Core.Symbol ident mt)
  else do
    m <- getInstantiated ident mt
    return (Core.Symbol m mt)
monomorph (Core.Application f p t) = do
  mt <- polyToMono t
  mf <- monomorph f
  mp <- monomorph p
  return (Core.Application mf mp mt)
monomorph (Core.NoArgApplication f t) = do
  mt <- polyToMono t
  mf <- monomorph f
  return (Core.NoArgApplication mf mt)
monomorph (Core.Fn a b t) = do
  mt <- polyToMono t
  mb <- withBinding a (monomorph b)
  return (Core.Fn a mb mt)
monomorph (Core.NoArgFn b t) = do
  mt <- polyToMono t
  mb <- monomorph b
  return (Core.NoArgFn mb mt)
monomorph (Core.Literal l t) = do
  mt <- polyToMono t
  return (Core.Literal l mt)
monomorph (Core.If cond then' else' t) = do
  mt <- polyToMono t
  mCond <- monomorph cond
  mThen <- monomorph then'
  mElse <- monomorph else'
  return (Core.If mCond mThen mElse mt)
monomorph (Core.Let name expr body t) = do
  mt <- polyToMono t
  mExpr <- monomorph expr
  mBody <- withBinding name (monomorph body)
  return (Core.Let name mExpr mBody mt)

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
      locals = MonomorphLocals Set.empty
  (_, s, _) <- runExcept $ runRWST (mapM_ monomorphDefinition definitions) locals st
  let is = Set.fromList (Map.elems (instances s))
      ms = Set.fromList (Map.elems (monomorphed s))
  return (CompiledPackage name imports is ms)

compile :: Env.Env -> Core.Package -> Either CompilationError CompiledPackage
compile = monomorphPackage

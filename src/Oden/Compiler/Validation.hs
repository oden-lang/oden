module Oden.Compiler.Validation where

import           Oden.Core             as Core
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName    (QualifiedName(..))
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Set        as Set
import qualified Data.Map        as Map

data ValidationError = Redefinition SourceInfo Identifier
                     | ValueDiscarded (Expr Type)
                     | DuplicatedRecordFieldName SourceInfo Identifier
                     | InvalidRow Type
                     deriving (Show, Eq, Ord)

data ValidationWarning = ValidationWarning -- There's no warnings defined yet.
                       deriving (Show, Eq, Ord)

type Validate = ReaderT
                (Set.Set Identifier)
                (StateT [ValidationWarning]
                        (Except ValidationError))

errorIfDefined :: Identifier -> Metadata SourceInfo -> Validate ()
errorIfDefined name (Metadata si) = do
  scope <- ask
  when (Set.member name scope) $
    throwError (Redefinition si name)

withIdentifier :: Identifier -> Validate a -> Validate a
withIdentifier = local . Set.insert

validateExpr :: Expr Type -> Validate ()
validateExpr Symbol{} = return ()
validateExpr (Subscript _ s i _) = do
  validateExpr s
  validateExpr i
validateExpr (Subslice _ s r _) = do
  validateExpr s
  validateRange r
validateExpr (UnaryOp _ _ rhs _) =
  validateExpr rhs
validateExpr (BinaryOp _ _ lhs rhs _) = do
  validateExpr lhs
  validateExpr rhs
validateExpr (Application _ f arg _) = do
  validateExpr f
  validateExpr arg
validateExpr (NoArgApplication _ f _) =
  validateExpr f
validateExpr (UncurriedFnApplication _ f args _) = do
  validateExpr f
  mapM_ validateExpr args
validateExpr (Fn _ (NameBinding si name) body _) =  do
  errorIfDefined name si
  withIdentifier name (validateExpr body)
validateExpr (NoArgFn _ body _) =
  validateExpr body
validateExpr (Let _ (NameBinding si name) value body _) = do
  errorIfDefined name si
  validateExpr value
  withIdentifier name (validateExpr body)
validateExpr Literal{} = return ()
validateExpr (Tuple _ f s r _) =
  mapM_ validateExpr (f:s:r)
validateExpr (Slice _ exprs _) =
  mapM_ validateExpr exprs
validateExpr (If _ c t e _) = do
  validateExpr c
  validateExpr t
  validateExpr e
validateExpr (Block _ exprs _) = do
  mapM_ warnOnDiscarded (init exprs)
  mapM_ validateExpr exprs
  where
  warnOnDiscarded :: Expr Type -> Validate ()
  warnOnDiscarded expr = case typeOf expr of
    (TCon _ (FQN [] (Identifier "unit"))) -> return ()
    _ -> throwError (ValueDiscarded expr)
validateExpr RecordInitializer{} = return ()
validateExpr (RecordFieldAccess _ expr _ _) = validateExpr expr
validateExpr (PackageMemberAccess _ _ _ _) = return ()

validateRange :: Range Type -> Validate()
validateRange (Range e1 e2) = do
  validateExpr e1
  validateExpr e2
validateRange (RangeTo e) =
  validateExpr e
validateRange (RangeFrom e) =
  validateExpr e

repeated :: [(Identifier, Type)] -> [(Identifier, Type)]
repeated fields = snd (foldl check (Set.empty, []) fields)
  where
  check (names, fields') f@(n, _)
    | n `Set.member` names = (names, fields'++ [f])
    | otherwise            = (Set.insert n names, fields')

validateType :: Type -> Validate ()
validateType (TTuple _ f s r) = mapM_ validateType (f:s:r)
validateType (TNoArgFn _ r) = validateType r
validateType (TFn _ d r) = mapM_ validateType [d, r]
validateType (TSlice _ t) = validateType t
validateType (TNamed _ _ t) = validateType t
validateType r@RExtension{} =
  case (repeated . Map.toAscList) <$> getFields r of
    Left _ -> throwError $ InvalidRow r
    Right ((name, _):_) -> throwError (DuplicatedRecordFieldName (getSourceInfo r) name)
    Right [] -> return ()
validateType _ = return ()

validatePackage :: Package -> Validate ()
validatePackage (Package _ _ definitions) = validateDefs definitions
  where
  validateDefs (Definition si name (_, expr):defs) = do
    errorIfDefined name si
    withIdentifier name $ do
      validateExpr expr
      validateDefs defs
  validateDefs (TypeDefinition _ (FQN _ n) _ type':defs) = do
    validateType type'
    withIdentifier n (validateDefs defs)
  validateDefs (_:defs) = validateDefs defs
  validateDefs [] = return ()

validate :: Package -> Either ValidationError [ValidationWarning]
validate pkg =
  runExcept (execStateT (runReaderT (validatePackage pkg) Set.empty) [])

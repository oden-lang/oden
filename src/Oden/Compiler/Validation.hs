module Oden.Compiler.Validation where

import           Oden.Core             as Core
import           Oden.Identifier
import           Oden.QualifiedName    (QualifiedName(..))
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Set        as Set

data ValidationError = Redefinition SourceInfo Name
                     | ValueDiscarded (Expr Type)
                     | DuplicatedStructFieldName SourceInfo Name
                     deriving (Show, Eq, Ord)

data ValidationWarning = ValidationWarning -- There's no warnings defined yet.
                       deriving (Show, Eq, Ord)

type Validate = ReaderT
                (Set.Set Name)
                (StateT [ValidationWarning]
                        (Except ValidationError))

errorIfDefined :: Name -> SourceInfo -> Validate ()
errorIfDefined name si = do
  scope <- ask
  when (Set.member name scope) $
    throwError (Redefinition si name)

withName :: Name -> Validate a -> Validate a
withName = local . Set.insert

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
  withName name (validateExpr body)
validateExpr (NoArgFn _ body _) =
  validateExpr body
validateExpr (Let _ (NameBinding si name) value body _) = do
  errorIfDefined name si
  validateExpr value
  withName name (validateExpr body)
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
    TUnit{} -> return ()
    _ -> throwError (ValueDiscarded expr)

validateRange :: Range Type -> Validate()
validateRange (Range e1 e2) = do
  validateExpr e1
  validateExpr e2
validateRange (RangeTo e) =
  validateExpr e
validateRange (RangeFrom e) =
  validateExpr e

validatePackage :: Package -> Validate ()
validatePackage (Package _ _ definitions) = validateDefs definitions
  where
  validateDefs (Definition si name (_, expr):defs) = do
    errorIfDefined name si
    withName name $ do
      validateExpr expr
      validateDefs defs
  validateDefs (StructDefinition _ (FQN _ n) _ fs:defs) = do
    case repeated fs of
      [] -> return ()
      (Core.StructField si name _:_) -> throwError (DuplicatedStructFieldName si name)
    withName n (validateDefs defs)
  validateDefs (_:defs) = validateDefs defs
  validateDefs [] = return ()

repeated :: [Core.StructField Type] -> [Core.StructField Type]
repeated fields = snd (foldl check (Set.empty, []) fields)
  where
  check (names, fields') f@(StructField _ n _)
    | n `Set.member` names = (names, fields'++ [f])
    | otherwise            = (Set.insert n names, fields')

validate :: Package -> Either ValidationError [ValidationWarning]
validate pkg =
  runExcept (execStateT (runReaderT (validatePackage pkg) Set.empty) [])

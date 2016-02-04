module Oden.Compiler.Validation where

import           Oden.Core
import           Oden.Identifier
import           Oden.Type.Polymorphic

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Set        as Set

data ValidationError = Redefinition Name
                     deriving (Show, Eq, Ord)

data ValidationWarning = ValueDiscarded (Expr Type)
                       deriving (Show, Eq, Ord)

type Validate = ReaderT
                (Set.Set Name)
                (StateT [ValidationWarning]
                        (Except ValidationError))

errorIfDefined :: Name -> Validate ()
errorIfDefined name = do
  scope <- ask
  when (Set.member name scope) $
    throwError (Redefinition name)

withName :: Name -> Validate a -> Validate a
withName = local . Set.insert

validateExpr :: Expr Type -> Validate ()
validateExpr Symbol{} = return ()
validateExpr (Op _ lhs rhs _) = do
  validateExpr lhs
  validateExpr rhs
validateExpr (Application f arg _) = do
  validateExpr f
  validateExpr arg
validateExpr (NoArgApplication f _) =
  validateExpr f
validateExpr (UncurriedFnApplication f args _) = do
  validateExpr f
  mapM_ validateExpr args
validateExpr (Fn name body _) =  do
  errorIfDefined name
  withName name (validateExpr body)
validateExpr (NoArgFn body _) =
  validateExpr body
validateExpr (Let name value body _) = do
  errorIfDefined name
  validateExpr value
  withName name (validateExpr body)
validateExpr (Literal _ _) = return ()
validateExpr (Tuple f s r _) =
  mapM_ validateExpr (f:s:r)
validateExpr (Slice exprs _) =
  mapM_ validateExpr exprs
validateExpr (If c t e _) = do
  validateExpr c
  validateExpr t
  validateExpr e
validateExpr (Block exprs _) =
  mapM_ validateExpr exprs

validatePackage :: Package -> Validate ()
validatePackage (Package _ _ definitions) = validateSeq definitions
  where
  validateSeq [] = return ()
  validateSeq (Definition name (_, expr) : ds) = do
    errorIfDefined name
    withName name $ do
      validateExpr expr
      validateSeq ds

validate :: Package -> Either ValidationError [ValidationWarning]
validate pkg =
  runExcept (execStateT (runReaderT (validatePackage pkg) Set.empty) [])

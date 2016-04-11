module Oden.Core.Expr where

import           Oden.Core.Operator
import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo

data NameBinding = NameBinding (Metadata SourceInfo) Identifier
                 deriving (Show, Eq, Ord)

data FieldInitializer r t =
  FieldInitializer (Metadata SourceInfo) Identifier (Expr r t)
  deriving (Show, Eq, Ord)

data Literal
  = Int Integer
  | Bool Bool
  | String String
  | Unit
  deriving (Show, Eq, Ord)

data Range e
  = Range e e
  | RangeTo e
  | RangeFrom e
  deriving (Show, Eq, Ord)

data Expr r t
  = Symbol (Metadata SourceInfo) Identifier t
  | Subscript (Metadata SourceInfo) (Expr r t) (Expr r t) t
  | Subslice (Metadata SourceInfo) (Expr r t) (Range (Expr r t)) t
  | UnaryOp (Metadata SourceInfo) UnaryOperator (Expr r t) t
  | BinaryOp (Metadata SourceInfo) BinaryOperator (Expr r t) (Expr r t) t
  | Application (Metadata SourceInfo) (Expr r t) (Expr r t) t
  | NoArgApplication (Metadata SourceInfo) (Expr r t) t
  | ForeignFnApplication (Metadata SourceInfo) (Expr r t) [Expr r t] t
  | Fn (Metadata SourceInfo) NameBinding (Expr r t) t
  | NoArgFn (Metadata SourceInfo) (Expr r t) t
  | Let (Metadata SourceInfo) NameBinding (Expr r t) (Expr r t) t
  | Literal (Metadata SourceInfo) Literal t
  | Tuple (Metadata SourceInfo) (Expr r t) (Expr r t) [Expr r t] t
  | If (Metadata SourceInfo) (Expr r t) (Expr r t) (Expr r t) t
  | Slice (Metadata SourceInfo) [Expr r t] t
  | Block (Metadata SourceInfo) [Expr r t] t
  | RecordInitializer (Metadata SourceInfo) t [FieldInitializer r t]
  | RecordFieldAccess (Metadata SourceInfo) (Expr r t) Identifier t
  | PackageMemberAccess (Metadata SourceInfo) Identifier Identifier t
  | MethodReference (Metadata SourceInfo) r t
  deriving (Show, Eq, Ord)

typeOf :: Expr r t -> t
typeOf expr = case expr of
  Symbol _ _ t                    -> t
  Subscript _ _ _ t               -> t
  Subslice _ _ _ t                -> t
  UnaryOp _ _ _ t                 -> t
  BinaryOp _ _ _ _ t              -> t
  Application _ _ _ t             -> t
  NoArgApplication _ _ t          -> t
  ForeignFnApplication _ _ _ t    -> t
  Fn _ _ _ t                      -> t
  NoArgFn _ _ t                   -> t
  Let _ _ _ _ t                   -> t
  Literal _ _ t                   -> t
  If _ _ _ _ t                    -> t
  Tuple _ _ _ _ t                 -> t
  Slice _ _ t                     -> t
  Block _ _ t                     -> t
  RecordInitializer _ t _         -> t
  RecordFieldAccess _ _ _ t       -> t
  PackageMemberAccess _ _ _ t     -> t
  MethodReference _ _ t           -> t

instance HasSourceInfo (Expr r t) where
  getSourceInfo expr = case expr of
    Symbol (Metadata si) _ _                    -> si
    Subscript (Metadata si) _ _ _               -> si
    Subslice (Metadata si) _ _ _                -> si
    UnaryOp (Metadata si) _ _ _                 -> si
    BinaryOp (Metadata si) _ _ _ _              -> si
    Application (Metadata si) _ _ _             -> si
    NoArgApplication (Metadata si) _ _          -> si
    ForeignFnApplication (Metadata si) _ _ _    -> si
    Fn (Metadata si) _ _ _                      -> si
    NoArgFn (Metadata si) _ _                   -> si
    Let (Metadata si) _ _ _ _                   -> si
    Literal (Metadata si) _ _                   -> si
    If (Metadata si) _ _ _ _                    -> si
    Slice (Metadata si) _ _                     -> si
    Tuple (Metadata si) _ _ _ _                 -> si
    Block (Metadata si) _ _                     -> si
    RecordInitializer (Metadata si) _ _         -> si
    RecordFieldAccess (Metadata si) _ _ _       -> si
    PackageMemberAccess (Metadata si) _ _ _     -> si
    MethodReference (Metadata si) _ _           -> si

  setSourceInfo si expr = case expr of
    Symbol _ i t                              -> Symbol (Metadata si) i t
    Subscript _ s i t                         -> Subscript (Metadata si) s i t
    Subslice _ s r t                          -> Subslice (Metadata si) s r t
    UnaryOp _ o r t                           -> UnaryOp (Metadata si) o r t
    BinaryOp _ p l r t                        -> BinaryOp (Metadata si) p l r t
    Application _ f a t                       -> Application (Metadata si) f a t
    NoArgApplication _ f t                    -> NoArgApplication (Metadata si) f t
    ForeignFnApplication _ f a t              -> ForeignFnApplication (Metadata si) f a t
    Fn _ n b t                                -> Fn (Metadata si) n b t
    NoArgFn _ b t                             -> NoArgFn (Metadata si) b t
    Let _ n v b t                             -> Let (Metadata si) n v b t
    Literal _ l t                             -> Literal (Metadata si) l t
    If _ c t e t'                             -> If (Metadata si) c t e t'
    Slice _ e t                               -> Slice (Metadata si) e t
    Tuple _ f s r t                           -> Tuple (Metadata si) f s r t
    Block _ e t                               -> Block (Metadata si) e t
    RecordInitializer _ t vs                  -> RecordInitializer (Metadata si) t vs
    RecordFieldAccess _ expr' name t          -> RecordFieldAccess (Metadata si) expr' name t
    PackageMemberAccess _ pkgAlias name t     -> PackageMemberAccess (Metadata si) pkgAlias name t
    MethodReference _ ref t                   -> MethodReference (Metadata si) ref t


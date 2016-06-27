{-# LANGUAGE LambdaCase #-}
module Oden.Core.Expr where

import           Oden.Core.Foreign
import           Oden.Identifier
import           Oden.Metadata
import           Oden.SourceInfo

data NameBinding = NameBinding (Metadata SourceInfo) Identifier
                 deriving (Show, Eq, Ord)

data FieldInitializer e =
  FieldInitializer (Metadata SourceInfo) Identifier e
  deriving (Show, Eq, Ord)

data Literal
  = Int Integer
  | Float Double
  | Bool Bool
  | String String
  | Unit
  deriving (Show, Eq, Ord)

data Range e
  = Range e e
  | RangeTo e
  | RangeFrom e
  deriving (Show, Eq, Ord)

data Expr r t m
  = Symbol (Metadata SourceInfo) Identifier t
  | Subscript (Metadata SourceInfo) (Expr r t m) (Expr r t m) t
  | Subslice (Metadata SourceInfo) (Expr r t m) (Range (Expr r t m)) t
  | Application (Metadata SourceInfo) (Expr r t m) (Expr r t m) t
  | NoArgApplication (Metadata SourceInfo) (Expr r t m) t
  | ForeignFnApplication (Metadata SourceInfo) (Expr r t m) [Expr r t m] t
  | Fn (Metadata SourceInfo) NameBinding (Expr r t m) t
  | NoArgFn (Metadata SourceInfo) (Expr r t m) t
  | Let (Metadata SourceInfo) NameBinding (Expr r t m) (Expr r t m) t
  | Literal (Metadata SourceInfo) Literal t
  | Tuple (Metadata SourceInfo) (Expr r t m) (Expr r t m) [Expr r t m] t
  | If (Metadata SourceInfo) (Expr r t m) (Expr r t m) (Expr r t m) t
  | Slice (Metadata SourceInfo) [Expr r t m] t
  | Block (Metadata SourceInfo) [Expr r t m] t
  | RecordInitializer (Metadata SourceInfo) [FieldInitializer (Expr r t m)] t
  | MemberAccess (Metadata SourceInfo) m t
  | MethodReference (Metadata SourceInfo) r t
  | Foreign (Metadata SourceInfo) ForeignExpr t
  deriving (Show, Eq, Ord)

typeOf :: Expr r t m -> t
typeOf expr = case expr of
  Symbol _ _ t                    -> t
  Subscript _ _ _ t               -> t
  Subslice _ _ _ t                -> t
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
  RecordInitializer _ _ t         -> t
  MemberAccess _ _ t              -> t
  MethodReference _ _ t           -> t
  Foreign _ _ t                   -> t

-- | Applies a mapping function to the type of an expression.
mapType :: (t -> t) -> Expr r t m -> Expr r t m
mapType f = \case
  Symbol meta i t                    -> Symbol meta i (f t)
  Subscript meta s i t               -> Subscript meta s i (f t)
  Subslice meta s r t                -> Subslice meta s r (f t)
  Application meta func a t          -> Application meta func a (f t)
  NoArgApplication meta func t       -> NoArgApplication meta func (f t)
  ForeignFnApplication meta func a t -> ForeignFnApplication meta func a (f t)
  Fn meta n b t                      -> Fn meta n b (f t)
  NoArgFn meta b t                   -> NoArgFn meta b (f t)
  Let meta n v b t                   -> Let meta n v b (f t)
  Literal meta l t                   -> Literal meta l (f t)
  If meta c t e t'                   -> If meta c t e (f t')
  Slice meta e t                     -> Slice meta e (f t)
  Tuple meta f' s r t                -> Tuple meta f' s r (f t)
  Block meta e t                     -> Block meta e (f t)
  RecordInitializer meta vs t        -> RecordInitializer meta vs (f t)
  MemberAccess meta member t         -> MemberAccess meta member (f t)
  MethodReference meta ref t         -> MethodReference meta ref (f t)
  Foreign meta foreign' t            -> Foreign meta foreign' (f t)

instance HasSourceInfo (Expr r t m) where
  getSourceInfo expr = case expr of
    Symbol (Metadata si) _ _                 -> si
    Subscript (Metadata si) _ _ _            -> si
    Subslice (Metadata si) _ _ _             -> si
    Application (Metadata si) _ _ _          -> si
    NoArgApplication (Metadata si) _ _       -> si
    ForeignFnApplication (Metadata si) _ _ _ -> si
    Fn (Metadata si) _ _ _                   -> si
    NoArgFn (Metadata si) _ _                -> si
    Let (Metadata si) _ _ _ _                -> si
    Literal (Metadata si) _ _                -> si
    If (Metadata si) _ _ _ _                 -> si
    Slice (Metadata si) _ _                  -> si
    Tuple (Metadata si) _ _ _ _              -> si
    Block (Metadata si) _ _                  -> si
    RecordInitializer (Metadata si) _ _      -> si
    MemberAccess (Metadata si) _ _           -> si
    MethodReference (Metadata si) _ _        -> si
    Foreign (Metadata si) _ _                -> si

  setSourceInfo si expr = case expr of
    Symbol _ i t                 -> Symbol (Metadata si) i t
    Subscript _ s i t            -> Subscript (Metadata si) s i t
    Subslice _ s r t             -> Subslice (Metadata si) s r t
    Application _ f a t          -> Application (Metadata si) f a t
    NoArgApplication _ f t       -> NoArgApplication (Metadata si) f t
    ForeignFnApplication _ f a t -> ForeignFnApplication (Metadata si) f a t
    Fn _ n b t                   -> Fn (Metadata si) n b t
    NoArgFn _ b t                -> NoArgFn (Metadata si) b t
    Let _ n v b t                -> Let (Metadata si) n v b t
    Literal _ l t                -> Literal (Metadata si) l t
    If _ c t e t'                -> If (Metadata si) c t e t'
    Slice _ e t                  -> Slice (Metadata si) e t
    Tuple _ f s r t              -> Tuple (Metadata si) f s r t
    Block _ e t                  -> Block (Metadata si) e t
    RecordInitializer _ t vs     -> RecordInitializer (Metadata si) t vs
    MemberAccess _ m t           -> MemberAccess (Metadata si) m t
    MethodReference _ ref t      -> MethodReference (Metadata si) ref t
    Foreign _ f t                -> Foreign (Metadata si) f t


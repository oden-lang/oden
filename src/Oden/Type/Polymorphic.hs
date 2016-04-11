{-# LANGUAGE TupleSections, FlexibleInstances #-}
-- | This module contains values representing polymorphic types, i.e. types
-- that can be instantiated into other polymorphic types and purely monomorphic
-- types.
module Oden.Type.Polymorphic (
  TVar(..),
  Type(..),
  TVarBinding(..),
  Scheme(..),
  ProtocolConstraint(..),
  ProtocolMethod(..),
  Protocol(..),
  rowToList,
  rowFromList,
  getLeafRow,
  uniqueRow,
  toMonomorphic,
  isPolymorphic,
  isPolymorphicType,
  FTV,
  ftv,
  getBindingVar,
  underlying
) where

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import qualified Oden.Type.Monomorphic as Mono

import           Data.List             (nub)
import qualified Data.Set              as Set

-- | Wrapper for type variable names.
newtype TVar = TV String
  deriving (Show, Eq, Ord)

data ProtocolConstraint = ProtocolConstraint (Metadata SourceInfo) Protocol Type
                        deriving (Show, Eq, Ord)

instance FTV ProtocolConstraint where
  ftv (ProtocolConstraint _ _ type') = ftv type'

-- | A polymorphic type.
data Type
  -- | A type variable.
  = TVar (Metadata SourceInfo) TVar
  -- | A tuple, with at least two elements.
  | TTuple (Metadata SourceInfo) Type Type [Type]
  -- | A type constructor.
  | TCon (Metadata SourceInfo) QualifiedName
  -- | Like a 'TFn' but with no argument, only a return type.
  | TNoArgFn (Metadata SourceInfo) Type
  -- | A curried function.
  | TFn (Metadata SourceInfo) Type Type
  -- | A slice type.
  | TSlice (Metadata SourceInfo) Type
  -- | A data structure, parameterized by a row for its fields.
  | TRecord (Metadata SourceInfo) Type
  -- | A name for a type, introduced by type definitions.
  | TNamed (Metadata SourceInfo) QualifiedName Type
  -- | A type constrained to implement protocols.
  | TConstrained (Set.Set ProtocolConstraint) Type

  -- | The empty row.
  | REmpty (Metadata SourceInfo)
  -- | A row extension (label and type extending another row).
  | RExtension (Metadata SourceInfo) Identifier Type Type

  -- | A foreign function. The last argument can be variadic (typed as a slice)
  -- and the function can have multiple return values (treated as a tuple by
  -- the type system and automatically wrapped into a tuple by codegen).
  | TForeignFn (Metadata SourceInfo) Bool [Type] [Type]

  deriving (Show, Eq, Ord)

instance HasSourceInfo Type where
  getSourceInfo (TVar (Metadata si) _)           = si
  getSourceInfo (TTuple (Metadata si) _ _ _)     = si
  getSourceInfo (TFn (Metadata si) _ _)          = si
  getSourceInfo (TNoArgFn (Metadata si) _)       = si
  getSourceInfo (TCon (Metadata si) _)           = si
  getSourceInfo (TSlice (Metadata si) _)         = si
  getSourceInfo (TRecord (Metadata si) _)        = si
  getSourceInfo (TNamed (Metadata si) _ _)       = si
  getSourceInfo (TConstrained _ t)               = getSourceInfo t
  getSourceInfo (REmpty (Metadata si))           = si
  getSourceInfo (RExtension (Metadata si) _ _ _) = si
  getSourceInfo (TForeignFn (Metadata si) _ _ _) = si

  setSourceInfo si (TVar _ v)            = TVar (Metadata si) v
  setSourceInfo si (TTuple _ f s r)      = TTuple (Metadata si) f s r
  setSourceInfo si (TFn _ p r)           = TFn (Metadata si) p r
  setSourceInfo si (TNoArgFn _ r)        = TNoArgFn (Metadata si) r
  setSourceInfo si (TCon _ n)            = TCon (Metadata si) n
  setSourceInfo si (TSlice _ t)          = TSlice (Metadata si) t
  setSourceInfo si (TRecord _ fs)        = TRecord (Metadata si) fs
  setSourceInfo si (TNamed _ n t)        = TNamed (Metadata si) n t
  setSourceInfo si (TConstrained cs t)   = TConstrained cs (setSourceInfo si t)
  setSourceInfo si REmpty{}              = REmpty (Metadata si)
  setSourceInfo si (RExtension _ l t r)  = RExtension (Metadata si) l t r
  setSourceInfo si (TForeignFn _ v p r)  = TForeignFn (Metadata si) v p r

-- | A type variable binding.
data TVarBinding = TVarBinding (Metadata SourceInfo) TVar
                 deriving (Show, Eq, Ord)

instance FTV TVarBinding where
  ftv (TVarBinding _ tv) = Set.singleton tv

getBindingVar :: TVarBinding -> TVar
getBindingVar (TVarBinding _ v)  = v

instance HasSourceInfo TVarBinding where
  getSourceInfo (TVarBinding (Metadata si) _)   = si
  setSourceInfo si (TVarBinding _ v) = TVarBinding (Metadata si) v

-- | A polymorphic type and its quantified type variable bindings.
data Scheme = Forall (Metadata SourceInfo) [TVarBinding] (Set.Set ProtocolConstraint) Type
            deriving (Show, Eq, Ord)

instance HasSourceInfo Scheme where
  getSourceInfo (Forall (Metadata si) _ _ _) = si
  setSourceInfo si (Forall _ v cs t) = Forall (Metadata si) v cs t

rowToList :: Type -> [(Identifier, Type)]
rowToList (RExtension _ label type' row) = (label, type') : rowToList row
rowToList _ = []

rowFromList :: [(Identifier, Type)] -> Type -> Type
rowFromList ((label, type') : fields) leaf = RExtension (Metadata Missing) label type' (rowFromList fields leaf)
rowFromList _ leaf = leaf

uniqueRow :: Type -> Type
uniqueRow row =
  let leaf = getLeafRow row
      deduplicated = nub (rowToList row)
  in rowFromList deduplicated leaf

getLeafRow :: Type -> Type
getLeafRow (RExtension _ _ _ row) = getLeafRow row
getLeafRow e = e

-- | Converts a polymorphic 'Type' to a monomorphic 'Mono.Type'
-- and fails if there's any 'TVar' in the type.
toMonomorphic :: Type -> Either String Mono.Type
toMonomorphic (TTuple si f s r) =
  Mono.TTuple si <$> toMonomorphic f
                 <*> toMonomorphic s
                 <*> mapM toMonomorphic r
toMonomorphic (TVar _ _) = Left "Cannot convert TVar to a monomorphic type"
toMonomorphic (TCon si n) = Right $ Mono.TCon si n
toMonomorphic (TNoArgFn si t) = Mono.TNoArgFn si <$> toMonomorphic t
toMonomorphic (TFn si tx ty) = Mono.TFn si <$> toMonomorphic tx
                                           <*> toMonomorphic ty
toMonomorphic (TForeignFn si variadic parameters returns) =
  Mono.TForeignFn si variadic <$> mapM toMonomorphic parameters
                              <*> mapM toMonomorphic returns
toMonomorphic (TSlice si t) = Mono.TSlice si <$> toMonomorphic t
toMonomorphic (TRecord si row) = Mono.TRecord si <$> toMonomorphic row
toMonomorphic (TNamed si n t) = Mono.TNamed si n <$> toMonomorphic t
toMonomorphic (TConstrained _ t) = toMonomorphic t
toMonomorphic (REmpty si) = return (Mono.REmpty si)
toMonomorphic (RExtension si label polyType row) =
  Mono.RExtension si label <$> toMonomorphic polyType <*> toMonomorphic row

-- | Predicate returning if there's any 'TVar' in the 'Scheme'.
isPolymorphic :: Scheme -> Bool
isPolymorphic (Forall _ tvars _ _) = not (null tvars)

-- TODO: Use 'toMonomorphic' here and check if it's a Left or Right value?
-- | Predicate returning if there's any 'TVar' in the 'Type'.
isPolymorphicType :: Type -> Bool
isPolymorphicType (TTuple _ f s r) = any isPolymorphicType (f:s:r)
isPolymorphicType (TVar _ _) = True
isPolymorphicType TCon{} = False
isPolymorphicType (TNoArgFn _ a) = isPolymorphicType a
isPolymorphicType (TFn _ p r) = isPolymorphicType p || isPolymorphicType r
isPolymorphicType (TForeignFn _ _ p r) =
  any isPolymorphicType p || any isPolymorphicType r
isPolymorphicType (TSlice _ a) = isPolymorphicType a
isPolymorphicType (TRecord _ r) = isPolymorphicType r
isPolymorphicType (TNamed _ _ t) = isPolymorphicType t
isPolymorphicType (TConstrained _ t) = isPolymorphicType t
isPolymorphicType (REmpty _) = False
isPolymorphicType (RExtension _ _ type' row) =
  isPolymorphicType type' || isPolymorphicType row

underlying :: Type -> Type
underlying (TNamed _ _ t) = underlying t
underlying t = t

-- | Values that can return the free type variables they contain.
class FTV a where
  ftv :: a -> Set.Set TVar

instance FTV Type where
  ftv (TTuple _ f s r)           = ftv (f:s:r)
  ftv TCon{}                     = Set.empty
  ftv (TVar _ a)                 = Set.singleton a
  ftv (TFn _ t1 t2)              = ftv t1 `Set.union` ftv t2
  ftv (TNoArgFn _ t)             = ftv t
  ftv (TForeignFn _ _ ps rs)     = ftv (ps ++ rs)
  ftv (TSlice _ t)               = ftv t
  ftv (TRecord _ r)              = ftv r
  ftv (TNamed _ _ t)             = ftv t
  ftv (TConstrained _ t)         = ftv t
  ftv (REmpty _)                 = Set.empty
  ftv (RExtension _ _ type' row) = ftv type' `Set.union` ftv row

instance FTV Scheme where
  ftv (Forall _ as _ t) =
    ftv t `Set.difference` Set.fromList (map getBindingVar as)

instance (FTV a) => FTV [a] where
  ftv = foldr (Set.union . ftv) Set.empty

instance (FTV a) => FTV (Set.Set a) where
  ftv = foldr (Set.union . ftv) Set.empty

data ProtocolMethod = ProtocolMethod (Metadata SourceInfo) Identifier Scheme
                      deriving (Show, Eq, Ord)

data Protocol = Protocol (Metadata SourceInfo) QualifiedName Type [ProtocolMethod]
              deriving (Show, Eq, Ord)

instance HasSourceInfo Protocol where
  getSourceInfo (Protocol (Metadata si) _ _ _)   = si
  setSourceInfo si (Protocol _ name var methods) =
    Protocol (Metadata si) name var methods

instance FTV ProtocolMethod where
  ftv (ProtocolMethod _ _ scheme) = ftv scheme

instance FTV Protocol where
  ftv (Protocol _ _ param methods) =
    ftv param `Set.union` ftv methods


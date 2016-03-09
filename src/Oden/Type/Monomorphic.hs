-- | This module contains values representing monomorphic types.
module Oden.Type.Monomorphic where

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Basic

data StructField = TStructField (Metadata SourceInfo) Identifier Type
                 deriving (Show, Eq, Ord)

data Type
  = TAny (Metadata SourceInfo)
  | TBasic (Metadata SourceInfo) BasicType
  | TUnit (Metadata SourceInfo)
  | TTuple (Metadata SourceInfo) Type Type [Type]
  | TCon (Metadata SourceInfo) Type Type
  | TNoArgFn (Metadata SourceInfo) Type
  | TFn (Metadata SourceInfo) Type Type
  | TSlice (Metadata SourceInfo) Type
  | TStruct (Metadata SourceInfo) [StructField]
  | TNamed (Metadata SourceInfo) QualifiedName Type
  | TUncurriedFn (Metadata SourceInfo) [Type] Type
  | TVariadicFn (Metadata SourceInfo) [Type] Type Type
  deriving (Show, Eq, Ord)

instance HasSourceInfo Type where
  getSourceInfo (TAny (Metadata si))              = si
  getSourceInfo (TBasic (Metadata si) _)          = si
  getSourceInfo (TUnit (Metadata si))             = si
  getSourceInfo (TTuple (Metadata si) _ _ _)      = si
  getSourceInfo (TFn (Metadata si) _ _)           = si
  getSourceInfo (TNoArgFn (Metadata si) _)        = si
  getSourceInfo (TCon (Metadata si) _ _)          = si
  getSourceInfo (TUncurriedFn (Metadata si) _ _)  = si
  getSourceInfo (TVariadicFn (Metadata si) _ _ _) = si
  getSourceInfo (TSlice (Metadata si) _)          = si
  getSourceInfo (TStruct (Metadata si) _)         = si
  getSourceInfo (TNamed (Metadata si) _ _)        = si

  setSourceInfo si (TAny _)              = TAny (Metadata si)
  setSourceInfo si (TBasic _ b)          = TBasic (Metadata si) b
  setSourceInfo si (TUnit _)             = TUnit (Metadata si)
  setSourceInfo si (TTuple _ f s r)      = TTuple (Metadata si) f s r
  setSourceInfo si (TFn _ a r)           = TFn (Metadata si) a r
  setSourceInfo si (TNoArgFn _ r)        = TNoArgFn (Metadata si) r
  setSourceInfo si (TCon _ d r)          = TCon (Metadata si) d r
  setSourceInfo si (TUncurriedFn _ a r)  = TUncurriedFn (Metadata si) a r
  setSourceInfo si (TVariadicFn _ a v r) = TVariadicFn (Metadata si) a v r
  setSourceInfo si (TSlice _ t)          = TSlice (Metadata si) t
  setSourceInfo si (TStruct _ fs)        = TStruct (Metadata si) fs
  setSourceInfo si (TNamed _ n t)        = TNamed (Metadata si) n t

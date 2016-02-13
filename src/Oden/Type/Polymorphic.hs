module Oden.Type.Polymorphic (
  TVar(..),
  Type(..),
  TVarBinding(..),
  Scheme(..),
  toMonomorphic,
  isPolymorphic,
  isPolymorphicType,
  FTV,
  ftv,
  getBindingVar
) where

import           Oden.Type.Basic
import qualified Oden.Type.Monomorphic as Mono
import           Oden.SourceInfo

import qualified Data.Set               as Set

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TAny SourceInfo
  -- | A type variable.
  | TBasic SourceInfo BasicType
  -- | A type variable.
  | TVar SourceInfo TVar
  -- | The unit type.
  | TUnit SourceInfo
  -- | A tuple, with at least two elements.
  | TTuple SourceInfo Type Type [Type]
  -- | A type constructor with a name.
  | TCon SourceInfo String
  -- | Like a 'TFn' but with no argument, only a return type.
  | TNoArgFn SourceInfo Type
  -- | A curried function.
  | TFn SourceInfo Type Type
  -- | A slice type.
  | TSlice SourceInfo Type

  -- For foreign definitions:

  -- | A function that can have multiple arguments (no currying).
  | TUncurriedFn SourceInfo [Type] Type -- TODO: Support multiple return values somehow.
  -- | A variadic function.
  | TVariadicFn SourceInfo [Type] Type Type -- TODO: Support multiple return values somehow.
  deriving (Show, Eq, Ord)

instance HasSourceInfo Type where
  getSourceInfo (TAny si)              = si
  getSourceInfo (TBasic si _)          = si
  getSourceInfo (TUnit si)             = si
  getSourceInfo (TVar si _)            = si
  getSourceInfo (TTuple si _ _ _)      = si
  getSourceInfo (TFn si _ _)           = si
  getSourceInfo (TNoArgFn si _)        = si
  getSourceInfo (TCon si _)            = si
  getSourceInfo (TUncurriedFn si _ _)  = si
  getSourceInfo (TVariadicFn si _ _ _) = si
  getSourceInfo (TSlice si _)          = si

  setSourceInfo si (TAny _)              = TAny si
  setSourceInfo si (TBasic _ b)          = TBasic si b
  setSourceInfo si (TUnit _)             = TUnit si
  setSourceInfo si (TVar _ v)            = TVar si v
  setSourceInfo si (TTuple _ f s r)      = TTuple si f s r
  setSourceInfo si (TFn _ a r)           = TFn si a r
  setSourceInfo si (TNoArgFn _ r)        = TNoArgFn si r
  setSourceInfo si (TCon _ s)            = TCon si s
  setSourceInfo si (TUncurriedFn _ a r)  = TUncurriedFn si a r
  setSourceInfo si (TVariadicFn _ a v r) = TVariadicFn si a v r
  setSourceInfo si (TSlice _ t)          = TSlice si t

data TVarBinding = TVarBinding SourceInfo TVar
                 deriving (Show, Eq, Ord)

getBindingVar :: TVarBinding -> TVar
getBindingVar (TVarBinding _ v)  = v

instance HasSourceInfo TVarBinding where
  getSourceInfo (TVarBinding si _)   = si
  setSourceInfo si (TVarBinding _ v) = TVarBinding si v

data Scheme = Forall SourceInfo [TVarBinding] Type
            deriving (Show, Eq, Ord)

instance HasSourceInfo Scheme where
  getSourceInfo (Forall si _ _) = si
  setSourceInfo si (Forall _ v t) = Forall si v t

toMonomorphic :: Type -> Either String Mono.Type
toMonomorphic (TAny si) = Right (Mono.TAny si)
toMonomorphic (TBasic si b) = Right (Mono.TBasic si b)
toMonomorphic (TUnit si) = Right (Mono.TUnit si)
toMonomorphic (TTuple si f s r) =
  Mono.TTuple si <$> toMonomorphic f
                 <*> toMonomorphic s
                 <*> mapM toMonomorphic r
toMonomorphic (TVar _ _) = Left "Cannot convert TVar to a monomorphic type"
toMonomorphic (TCon si s) = Right (Mono.TCon si s)
toMonomorphic (TNoArgFn si t) = Mono.TNoArgFn si <$> toMonomorphic t
toMonomorphic (TFn si tx ty) = Mono.TFn si <$> toMonomorphic tx
                                           <*> toMonomorphic ty
toMonomorphic (TUncurriedFn si a r) =
  Mono.TUncurriedFn si <$> mapM toMonomorphic a
                       <*> toMonomorphic r
toMonomorphic (TVariadicFn si a v r) =
  Mono.TVariadicFn si <$> mapM toMonomorphic a
                      <*> toMonomorphic v
                      <*> toMonomorphic r
toMonomorphic (TSlice si t) = Mono.TSlice si <$> toMonomorphic t

isPolymorphic :: Scheme -> Bool
isPolymorphic (Forall _ tvars _) = not (null tvars)

isPolymorphicType :: Type -> Bool
isPolymorphicType TAny{} = False
isPolymorphicType TUnit{} = False
isPolymorphicType TBasic{} = False
isPolymorphicType (TTuple _ f s r) = any isPolymorphicType (f:s:r)
isPolymorphicType (TVar _ _) = True
isPolymorphicType (TCon _ _) = False
isPolymorphicType (TNoArgFn _ a) = isPolymorphicType a
isPolymorphicType (TFn _ a b) = isPolymorphicType a || isPolymorphicType b
isPolymorphicType (TUncurriedFn _ a r) =
  any isPolymorphicType a || isPolymorphicType r
isPolymorphicType (TVariadicFn _ a v r) =
  any isPolymorphicType a || isPolymorphicType v || isPolymorphicType r
isPolymorphicType (TSlice _ a) = isPolymorphicType a

class FTV a where
  ftv :: a -> Set.Set TVar

instance FTV Type where
  ftv TAny{}                    = Set.empty
  ftv TBasic{}                  = Set.empty
  ftv TUnit{}                   = Set.empty
  ftv (TTuple _ f s r)          = foldl Set.union (ftv f) (ftv s : map ftv r)
  ftv TCon{}                    = Set.empty
  ftv (TVar _ a)                = Set.singleton a
  ftv (TFn _ t1 t2)             = ftv t1 `Set.union` ftv t2
  ftv (TNoArgFn _ t)            = ftv t
  ftv (TUncurriedFn _ as r)     = foldl Set.union (ftv r) (map ftv as)
  ftv (TVariadicFn _ as v r)    = foldl Set.union (ftv r) (ftv v : map ftv as)
  ftv (TSlice _ t)              = ftv t

instance FTV Scheme where
  ftv (Forall _ as t) =
    ftv t `Set.difference` Set.fromList (map getBindingVar as)

instance FTV a => FTV [a] where
  ftv   = foldr (Set.union . ftv) Set.empty

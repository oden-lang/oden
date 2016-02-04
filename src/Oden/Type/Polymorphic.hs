module Oden.Type.Polymorphic (
  TVar(..),
  Type(..),
  Scheme(..),
  toMonomorphic,
  isPolymorphic,
  isPolymorphicType,
  typeInt,
  typeBool,
  typeString,
  FTV,
  ftv
) where

import qualified Oden.Type.Monomorphic as Mono

import qualified Data.Set               as Set
import           Data.List

newtype TVar = TV String
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = '#':s

data Type
  = TAny
  -- | A type variable.
  | TVar TVar
  -- | The unit type.
  | TUnit
  -- | A tuple, with at least two elements.
  | TTuple Type Type [Type]
  -- | A type constructor with a name.
  | TCon String
  -- | Like a 'TFn' but with no argument, only a return type.
  | TNoArgFn Type
  -- | A curried function.
  | TFn Type Type
  -- | A slice type.
  | TSlice Type

  -- For foreign definitions:

  -- | A function that can have multiple arguments (no currying).
  | TUncurriedFn [Type] Type -- TODO: Support multiple return values somehow.
  -- | A variadic function.
  | TVariadicFn [Type] Type Type -- TODO: Support multiple return values somehow.

  deriving (Eq, Ord)

instance Show Type where
  show TAny = "any"
  show TUnit = "()"
  show (TTuple fst' snd' rest) =
    "(" ++ intercalate ", " (map show (fst' : snd' : rest)) ++ ")"
  show (TFn a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TNoArgFn a) = "(-> " ++ show a ++ ")"
  show (TVar a) = show a
  show (TCon a) = a
  show (TUncurriedFn as rs) =
    "(uncurried-fn (" ++ unwords (map show as) ++ ") " ++ show rs ++ ")"
  show (TVariadicFn as v rs) =
    "(variadic-fn (" ++ intercalate "," (map show as)
    ++ " (* " ++ show v ++ ")) " ++ show rs ++ ")"
  show (TSlice t) = "!(" ++ show t ++ ")"

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vs t) = "forall " ++ (unwords (map show vars)) ++ ". " ++ show t
    where vars = unwords (map show vs)

toMonomorphic :: Type -> Either String Mono.Type
toMonomorphic TAny = Right Mono.TAny
toMonomorphic TUnit = Right Mono.TUnit
toMonomorphic (TTuple f s r) =
  Mono.TTuple <$> toMonomorphic f
              <*> toMonomorphic s
              <*> mapM toMonomorphic r
toMonomorphic (TVar _) = Left "Cannot convert TVar to a monomorphic type"
toMonomorphic (TCon s) = Right (Mono.TCon s)
toMonomorphic (TNoArgFn t) = Mono.TNoArgFn <$> toMonomorphic t
toMonomorphic (TFn tx ty) = Mono.TFn <$> toMonomorphic tx <*> toMonomorphic ty
toMonomorphic (TUncurriedFn a r) =
  Mono.TUncurriedFn <$> mapM toMonomorphic a
                    <*> toMonomorphic r
toMonomorphic (TVariadicFn a v r) =
  Mono.TVariadicFn <$> mapM toMonomorphic a
                   <*> toMonomorphic v
                   <*> toMonomorphic r
toMonomorphic (TSlice t) = Mono.TSlice <$> toMonomorphic t

isPolymorphic :: Scheme -> Bool
isPolymorphic (Forall tvars _) = not (null tvars)

isPolymorphicType :: Type -> Bool
isPolymorphicType TAny = False
isPolymorphicType TUnit = False
isPolymorphicType (TTuple f s r) = any isPolymorphicType (f:s:r)
isPolymorphicType (TVar _) = True
isPolymorphicType (TCon _) = False
isPolymorphicType (TNoArgFn a) = isPolymorphicType a
isPolymorphicType (TFn a b) = isPolymorphicType a || isPolymorphicType b
isPolymorphicType (TUncurriedFn a r) =
  any isPolymorphicType a || isPolymorphicType r
isPolymorphicType (TVariadicFn a v r) =
  any isPolymorphicType a || isPolymorphicType v || isPolymorphicType r
isPolymorphicType (TSlice a) = isPolymorphicType a

typeInt, typeBool, typeString :: Type
typeInt  = TCon "int"
typeBool = TCon "bool"
typeString = TCon "string"

class FTV a where
  ftv :: a -> Set.Set TVar

instance FTV Type where
  ftv TAny                      = Set.empty
  ftv TUnit                     = Set.empty
  ftv (TTuple f s r)            = foldl Set.union (ftv f) (ftv s : map ftv r)
  ftv TCon{}                    = Set.empty
  ftv (TVar a)                  = Set.singleton a
  ftv (t1 `TFn` t2)             = ftv t1 `Set.union` ftv t2
  ftv (TNoArgFn t)              = ftv t
  ftv (TUncurriedFn as r)       = foldl Set.union (ftv r) (map ftv as)
  ftv (TVariadicFn as v r)      = foldl Set.union (ftv r) (ftv v : map ftv as)
  ftv (TSlice t)                = ftv t

instance FTV Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance FTV a => FTV [a] where
  ftv   = foldr (Set.union . ftv) Set.empty

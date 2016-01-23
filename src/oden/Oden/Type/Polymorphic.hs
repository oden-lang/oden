module Oden.Type.Polymorphic (
  TVar(..),
  Type(..),
  Scheme(..),
  toMonomorphic,
  isPolymorphic,
  isPolymorphicType,
  typeInt,
  typeBool,
  typeUnit,
  typeString,
  FTV,
  ftv
) where

import qualified Oden.Type.Monomorphic as Mono

import qualified Data.Set               as Set

newtype TVar = TV String
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = '#':s

data Type
  = TAny
  -- | A type variable.
  | TVar TVar
  -- | A type constructor with a name.
  | TCon String
  -- | Like a 'TFn' but with no argument, only a return type.
  | TNoArgFn Type
  -- | A curried function.
  | TFn Type Type
  -- | A function that can have multiple arguments (no currying).
  --   TODO: Support multiple return values somehow.
  | TUncurriedFn [Type] Type
  -- | A variadic function.
  | TVariadicFn [Type] Type Type -- TODO: Support multiple return values somehow.
  -- | A slice type.
  | TSlice Type
  deriving (Eq, Ord)

instance Show Type where
  show TAny = "any"
  show (TFn a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TNoArgFn a) = "(-> " ++ show a ++ ")"
  show (TVar a) = show a
  show (TCon a) = a
  show (TUncurriedFn as rs) = "(uncurried-fn (" ++ unwords (map show as) ++ ") " ++ show rs ++ ")"
  show (TVariadicFn as v rs) =
    "(variadic-fn (" ++ unwords (map show as) ++ " (* " ++ show v ++ ")) " ++ show rs ++ ")"
  show (TSlice t) = "!(" ++ show t ++ ")"

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vs t) = "(forall (" ++ vars ++ ") " ++ show t ++ ")"
    where vars = unwords (map show vs)

toMonomorphic :: Type -> Either String Mono.Type
toMonomorphic TAny = Right Mono.TAny
toMonomorphic (TVar _) = Left "Cannot convert TVar to a monomorphic type"
toMonomorphic (TCon s) = Right (Mono.TCon s)
toMonomorphic (TNoArgFn t) = Mono.TNoArgFn <$> toMonomorphic t
toMonomorphic (TFn tx ty) = Mono.TFn <$> toMonomorphic tx <*> toMonomorphic ty
toMonomorphic (TUncurriedFn a r) = Mono.TUncurriedFn <$> mapM toMonomorphic a <*> toMonomorphic r
toMonomorphic (TVariadicFn a v r) = Mono.TVariadicFn <$> mapM toMonomorphic a <*> toMonomorphic v <*> toMonomorphic r
toMonomorphic (TSlice t) = Mono.TSlice <$> toMonomorphic t

isPolymorphic :: Scheme -> Bool
isPolymorphic (Forall tvars _) = not (null tvars)

isPolymorphicType :: Type -> Bool
isPolymorphicType TAny = False
isPolymorphicType (TVar _) = True
isPolymorphicType (TCon _) = False
isPolymorphicType (TNoArgFn a) = isPolymorphicType a
isPolymorphicType (TFn a b) = isPolymorphicType a || isPolymorphicType b
isPolymorphicType (TUncurriedFn a r) = any isPolymorphicType (r:a)
isPolymorphicType (TVariadicFn a v r) = any isPolymorphicType (r:v:a)
isPolymorphicType (TSlice a) = isPolymorphicType a

typeInt, typeBool, typeUnit, typeString :: Type
typeInt  = TCon "int"
typeBool = TCon "bool"
typeUnit = TCon "unit"
typeString = TCon "string"

class FTV a where
  ftv :: a -> Set.Set TVar

instance FTV Type where
  ftv TAny                      = Set.empty
  ftv TCon{}                    = Set.empty
  ftv (TVar a)                  = Set.singleton a
  ftv (t1 `TFn` t2)            = ftv t1 `Set.union` ftv t2
  ftv (TNoArgFn t)            = ftv t
  ftv (TUncurriedFn as r)            = foldl Set.union (ftv r) (map ftv as)
  ftv (TVariadicFn as v r)  = foldl Set.union (ftv r) (ftv v : map ftv as)
  ftv (TSlice t)                = ftv t

instance FTV Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance FTV a => FTV [a] where
  ftv   = foldr (Set.union . ftv) Set.empty

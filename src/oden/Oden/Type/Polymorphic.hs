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
  typeString
) where

import qualified Oden.Type.Monomorphic as Mono

newtype TVar = TV String
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = '#':s

data Type
  -- | A type variable.
  = TVar TVar
  -- | A type constructor with a name.
  | TCon String
  -- | Like a 'TArr' but with no argument, only a return type.
  | TArrSingle Type
  -- | A TArr (arrow) represents a Oden function (with currying).
  | TArr Type Type
  -- | Represents a Go func and can have multiple arguments (without
  -- currying).
  | TGoFunc [Type] Type -- TODO: Support multiple return values somehow.
  -- | Represents a variadic Go func.
  | TVariadicGoFunc [Type] Type Type -- TODO: Support multiple return values somehow.
  -- | A Go slice type.
  | TSlice Type
  deriving (Eq, Ord)

instance Show Type where
  show (TArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TArrSingle a) = "(-> " ++ show a ++ ")"
  show (TVar a) = show a
  show (TCon a) = a
  show (TGoFunc as rs) = "(go-func (" ++ unwords (map show as) ++ ") " ++ show rs ++ ")"
  show (TVariadicGoFunc as v rs) =
    "(go-func (" ++ unwords (map show as) ++ " (* " ++ show v ++ ")) " ++ show rs ++ ")"
  show (TSlice t) = "!(" ++ show t ++ ")"

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vs t) = "(forall (" ++ vars ++ ") " ++ show t ++ ")"
    where vars = unwords (map show vs)

toMonomorphic :: Type -> Either String Mono.Type
toMonomorphic (TVar _) = Left "Cannot convert TVar to a monomorphic type"
toMonomorphic (TCon s) = Right (Mono.TCon s)
toMonomorphic (TArrSingle t) = Mono.TArrSingle <$> toMonomorphic t
toMonomorphic (TArr tx ty) = Mono.TArr <$> toMonomorphic tx <*> toMonomorphic ty
toMonomorphic (TGoFunc a r) = Mono.TGoFunc <$> mapM toMonomorphic a <*> toMonomorphic r
toMonomorphic (TVariadicGoFunc a v r) = Mono.TVariadicGoFunc <$> mapM toMonomorphic a <*> toMonomorphic v <*> toMonomorphic r
toMonomorphic (TSlice t) = Mono.TSlice <$> toMonomorphic t

isPolymorphic :: Scheme -> Bool
isPolymorphic (Forall tvars _) = not (null tvars)

isPolymorphicType :: Type -> Bool
isPolymorphicType (TVar _) = True
isPolymorphicType (TCon _) = False
isPolymorphicType (TArrSingle a) = isPolymorphicType a
isPolymorphicType (TArr a b) = isPolymorphicType a || isPolymorphicType b
isPolymorphicType (TGoFunc a r) = any isPolymorphicType (r:a)
isPolymorphicType (TVariadicGoFunc a v r) = any isPolymorphicType (r:v:a)
isPolymorphicType (TSlice a) = isPolymorphicType a

typeInt, typeBool, typeUnit, typeString :: Type
typeInt  = TCon "int"
typeBool = TCon "bool"
typeUnit = TCon "unit"
typeString = TCon "string"

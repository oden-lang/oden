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
  = TVar TVar
  | TCon String
  | TArrSingle Type
  | TArr Type Type
  deriving (Eq, Ord)

instance Show Type where
  show (TArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TArrSingle a) = "(-> " ++ show a ++ ")"
  show (TVar a) = show a
  show (TCon a) = a

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

isPolymorphic :: Scheme -> Bool
isPolymorphic (Forall tvars _) = not (null tvars)

isPolymorphicType :: Type -> Bool
isPolymorphicType (TVar _) = True
isPolymorphicType (TCon _) = False
isPolymorphicType (TArrSingle a) = isPolymorphicType a
isPolymorphicType (TArr a b) = isPolymorphicType a || isPolymorphicType b

typeInt, typeBool, typeUnit, typeString :: Type
typeInt  = TCon "int"
typeBool = TCon "bool"
typeUnit = TCon "unit"
typeString = TCon "string"

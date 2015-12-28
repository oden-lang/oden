module Oden.Type.Polymorphic where

import qualified Oden.Type.Monomorphic as Mono

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArrSingle Type
  | TArr Type Type
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

toMonomorphic :: Type -> Either String Mono.Type
toMonomorphic (TVar _) = Left "Cannot convert TVar to a monomorphic type"
toMonomorphic (TCon s) = Right (Mono.TCon s)
toMonomorphic (TArrSingle t) = Mono.TArrSingle <$> toMonomorphic t
toMonomorphic (TArr tx ty) = Mono.TArr <$> toMonomorphic tx <*> toMonomorphic ty

isPolymorphic :: Scheme -> Bool
isPolymorphic (Forall tvars _) = not (null tvars)

typeInt, typeBool, typeUnit, typeString :: Type
typeInt  = TCon "int"
typeBool = TCon "bool"
typeUnit = TCon "unit"
typeString = TCon "string"

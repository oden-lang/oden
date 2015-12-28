module Oden.Type.Monomorphic where

data Type
  = TCon String
  | TArrSingle Type
  | TArr Type Type
  deriving (Show, Eq, Ord)

typeInt, typeBool, typeUnit, typeString :: Type
typeInt  = TCon "int"
typeBool = TCon "bool"
typeUnit = TCon "unit"
typeString = TCon "string"

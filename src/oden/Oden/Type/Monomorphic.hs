module Oden.Type.Monomorphic where

data Type
  = TAny
  | TCon String
  | TArrSingle Type
  | TArr Type Type
  | TGoFunc [Type] Type
  | TVariadicGoFunc [Type] Type Type
  | TSlice Type
  deriving (Eq, Ord)

parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s

instance Show Type where
  show TAny = "any"
  show (TArr a b) = parensIf (isArrow a) (show a) ++ " -> " ++ show b
    where
      isArrow TArr{} = True
      isArrow _ = False
  show (TArrSingle a) = "(-> " ++ show a ++ ")"
  show (TCon a) = a
  show (TGoFunc as rs) = "(go-func (" ++ unwords (map show as) ++ ") " ++ show rs ++ ")"
  show (TVariadicGoFunc as v rs) =
    "(go-func (" ++ unwords (map show as) ++ " (* " ++ show v ++ ")) " ++ show rs ++ ")"
  show (TSlice t) = "!(" ++ show t ++ ")"

typeInt, typeBool, typeUnit, typeString :: Type
typeInt  = TCon "int"
typeBool = TCon "bool"
typeUnit = TCon "unit"
typeString = TCon "string"

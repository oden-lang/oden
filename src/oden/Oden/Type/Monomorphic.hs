module Oden.Type.Monomorphic where

data Type
  = TCon String
  | TArrSingle Type
  | TArr Type Type
  deriving (Eq, Ord)

parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s

instance Show Type where
  show (TArr a b) = parensIf (isArrow a) (show a) ++ " -> " ++ show b
    where
      isArrow TArr{} = True
      isArrow _ = False
  show (TCon a) = a

typeInt, typeBool, typeUnit, typeString :: Type
typeInt  = TCon "int"
typeBool = TCon "bool"
typeUnit = TCon "unit"
typeString = TCon "string"

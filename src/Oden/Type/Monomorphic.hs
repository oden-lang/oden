module Oden.Type.Monomorphic where

data Type
  = TAny
  | TCon String
  | TNoArgFn Type
  | TFn Type Type
  | TUncurriedFn [Type] Type
  | TVariadicFn [Type] Type Type
  | TSlice Type
  deriving (Eq, Ord)

parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s

instance Show Type where
  show TAny = "any"
  show (TFn a b) = parensIf (isArrow a) (show a) ++ " -> " ++ show b
    where
      isArrow TFn{} = True
      isArrow _ = False
  show (TNoArgFn a) = "(-> " ++ show a ++ ")"
  show (TCon a) = a
  show (TUncurriedFn as rs) = "(uncurried-fn (" ++ unwords (map show as) ++ ") " ++ show rs ++ ")"
  show (TVariadicFn as v rs) =
    "(variadic-fn (" ++ unwords (map show as) ++ " (* " ++ show v ++ ")) " ++ show rs ++ ")"
  show (TSlice t) = "!(" ++ show t ++ ")"

typeInt, typeBool, typeUnit, typeString :: Type
typeInt  = TCon "int"
typeBool = TCon "bool"
typeUnit = TCon "unit"
typeString = TCon "string"

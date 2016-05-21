module Oden.Core.Operator where

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Equals
                    | NotEquals
                    | Concat
                    | LessThan
                    | GreaterThan
                    | LessThanEqual
                    | GreaterThanEqual
                    | And
                    | Or
                    deriving (Eq, Ord)

data UnaryOperator = Negative
                   | Positive
                   | Not
                   deriving (Eq, Ord)

instance Show BinaryOperator where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"
  show Equals = "=="
  show NotEquals = "/="
  show Concat = "++"
  show LessThan = "<"
  show GreaterThan = ">"
  show LessThanEqual = "<="
  show GreaterThanEqual = ">="
  show And = "&&"
  show Or = "||"

instance Show UnaryOperator where
  show Negative = "-"
  show Positive = "+"
  show Not = "!"

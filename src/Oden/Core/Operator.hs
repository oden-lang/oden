module Oden.Core.Operator where

data BinaryOperator = Add
                    | Subtract
                    | Multiply
                    | Divide
                    | Equals
                    | Concat
                    | LessThan
                    | GreaterThan
                    | LessThanEqual
                    | GreaterThanEqual
                    | And
                    | Or
                    deriving (Show, Eq, Ord)

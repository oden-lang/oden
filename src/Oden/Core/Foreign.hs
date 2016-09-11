module Oden.Core.Foreign where

import           Oden.Identifier

data UnaryOperator
  = Negate
  | Not
  deriving (Eq, Ord)

instance Show UnaryOperator where
  show Negate = "-"
  show Not = "!"

data BinaryOperator
  = Add
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

data ForeignExpr
  = ForeignBinaryOperator BinaryOperator
  | ForeignUnaryOperator UnaryOperator
  | ForeignSymbol Identifier
  | BidirectionalChannel
  | Receiver
  | Sender
  deriving (Show, Eq, Ord)

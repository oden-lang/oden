module Oden.Go.Types where

data Type = Basic String Bool
          | Pointer Type
          | Array Int Type
          | Slice Type
          | Signature Bool (Maybe Type) [Type] [Type]
          | Named String String Type
          | Unsupported String
          deriving (Show, Eq)


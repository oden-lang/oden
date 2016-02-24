module Oden.Go.Types where

data StructField = StructField String Type
                 deriving (Show, Eq)

data Type = Basic String Bool
          | Pointer Type
          | Array Int Type
          | Slice Type
          | Signature Bool (Maybe Type) [Type] [Type]
          | Struct [StructField]
          | Named [String] String Type
          | Interface
          | Unsupported String
          deriving (Show, Eq)


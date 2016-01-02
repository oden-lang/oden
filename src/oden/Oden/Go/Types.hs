module Oden.Go.Types where

import           Data.Aeson

data Type = Basic String
          | Pointer Type
          | Array Int Type
          | Slice Type
          | Signature (Maybe Type) [Type] [Type]
          | Named String String Type
          | Unsupported String
          deriving (Show, Eq)


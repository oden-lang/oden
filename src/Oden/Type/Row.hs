module Oden.Type.Row where

import Oden.Identifier
import Oden.Metadata
import Oden.SourceInfo

import Data.Set as Set

data Field t = Field (Metadata SourceInfo) Identifier t
             deriving (Show, Eq, Ord)

fieldType :: Field t -> t
fieldType (Field _ _ t) = t

data Row t = EmptyRow
           | Extension (Metadata SourceInfo) (Field t) (Row t)
           deriving (Show, Eq, Ord)

fields :: Ord t => Row t -> Set (Field t)
fields EmptyRow = Set.empty
fields (Extension _ f r) = Set.insert f (fields r)

module Oden.SourceInfo where

type Line = Int
type Column = Int

data Position = Position { fileName :: FilePath
                         , line :: Line
                         , column :: Column }
                deriving (Show, Eq, Ord)

data SourceInfo = SourceInfo { position :: Position }
                | Predefined
                | Missing
                deriving (Show, Eq, Ord)

class HasSourceInfo t where
  getSourceInfo :: t -> SourceInfo
  setSourceInfo :: SourceInfo -> t -> t

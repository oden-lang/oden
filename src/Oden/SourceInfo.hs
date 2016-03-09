module Oden.SourceInfo where

type Line = Int
type Column = Int

data Position = Position { fileName :: FilePath
                         , line :: Line
                         , column :: Column }
                deriving (Eq, Ord)

instance Show Position where
  show (Position f l c) = f ++ ":" ++ show l ++ ":" ++ show c

data SourceInfo = SourceInfo { position :: Position }
                | Predefined
                | Missing
                deriving (Eq, Ord)

instance Show SourceInfo where
  show Predefined = "<predefined>"
  show Missing = "<missing>"
  show (SourceInfo pos) = show pos

-- | Types from which you can get and set 'SourceInfo'.
class HasSourceInfo t where
  getSourceInfo :: t -> SourceInfo
  setSourceInfo :: SourceInfo -> t -> t

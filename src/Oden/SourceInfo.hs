module Oden.SourceInfo where

type Line = Int
type Column = Int

data Position = Position { fileName :: FilePath
                         , line :: Line
                         , column :: Column }

data SourceInfo = SourceInfo { position :: Position }

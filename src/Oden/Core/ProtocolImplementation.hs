module Oden.Core.ProtocolImplementation where

import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

data MethodImplementation e
  = MethodImplementation (Metadata SourceInfo) ProtocolMethod e
  deriving (Show, Eq, Ord)

data ProtocolImplementation e
  = ProtocolImplementation (Metadata SourceInfo) Protocol [MethodImplementation e]
  deriving (Show, Eq, Ord)

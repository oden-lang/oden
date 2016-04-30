module Oden.Core.ProtocolImplementation where

import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

data MethodImplementation e
  = MethodImplementation (Metadata SourceInfo) ProtocolMethod e
  deriving (Show, Eq, Ord)

data ProtocolImplementation e
  -- TODO: Add target type
  = ProtocolImplementation (Metadata SourceInfo) Protocol [MethodImplementation e]
  deriving (Show, Eq, Ord)

instance FTV e => FTV (MethodImplementation e) where
  ftv (MethodImplementation _ _ e) = ftv e

instance FTV e => FTV (ProtocolImplementation e) where
  ftv (ProtocolImplementation _ _ methods) = ftv methods

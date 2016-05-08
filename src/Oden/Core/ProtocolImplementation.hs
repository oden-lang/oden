module Oden.Core.ProtocolImplementation where

import           Oden.Metadata
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

data MethodImplementation e
  = MethodImplementation (Metadata SourceInfo) MethodName e
  deriving (Show, Eq, Ord)

data ProtocolImplementation e
  = ProtocolImplementation (Metadata SourceInfo) ProtocolName Type [MethodImplementation e]
  deriving (Show, Eq, Ord)

instance FTV e => FTV (MethodImplementation e) where
  ftv (MethodImplementation _ _ e) = ftv e

instance FTV e => FTV (ProtocolImplementation e) where
  ftv (ProtocolImplementation _ _ _ methods) = ftv methods


module Oden.Type.Protocol where

import Oden.Identifier
import Oden.Metadata
import Oden.SourceInfo
import Oden.QualifiedName
import Oden.Type.Polymorphic

data ProtocolFunction = ProtocolFunction (Metadata SourceInfo) Identifier Scheme
                      deriving (Show, Eq, Ord)

data Protocol = Protocol (Metadata SourceInfo) QualifiedName TVarBinding [ProtocolFunction]
              deriving (Show, Eq, Ord)

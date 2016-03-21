module Oden.Type.Kind where

import Oden.Type.Polymorphic

data Kind = Star
          | Row
          | FunctionKind
          deriving (Show, Eq, Ord)

kindOf :: Type -> Kind
kindOf REmpty{}= Row
kindOf RExtension{} = Row
kindOf _ = Star

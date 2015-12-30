module Oden.Output.Infer where

import Text.PrettyPrint

import Oden.Output
import Oden.Infer

instance OdenOutput TypeError where
  outputType _ = Error

  name (UnificationFail _ _)      = "Infer.UnificationFail"
  name (InfiniteType _ _)         = "Infer.InfiniteType"
  name (NotInScope _)             = "Infer.NotInScope"
  name (Ambigious _)              = "Infer.Ambigious"
  name (UnificationMismatch _ _)  = "Infer.UnificationMismatch"

  header (UnificationFail t1 t2) = text "Cannot unify types" <+> code t1 <+> text "and" <+> code t2
  header (InfiniteType v t) = text "Cannot construct an infinite type"
  header (NotInScope i) = code i <+> text "is not in scope"
  header (Ambigious cs) = text "Cannot match types"
  header (UnificationMismatch ts1 ts2) = text "Types does not match"

  details (UnificationFail t1 t2) = empty
  details (InfiniteType v t) = code v <+> equals <+> code t
  details (NotInScope i) = empty
  details (Ambigious cs) = vcat (map formatConstraint cs)
    where formatConstraint (t1, t2) = text "Expected" <+> code t1 <+> text "but got" <+> code t2
  details (UnificationMismatch ts1 ts2) = vcat (zipWith formatTypes ts1 ts2)
    where formatTypes t1 t2 | t1 == t2 = code t1 <+> text "==" <+> code t2
          formatTypes t1 t2 = code t1 <+> text "!=" <+> code t2


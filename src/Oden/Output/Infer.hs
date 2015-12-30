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

  header (UnificationFail t1 t2) s = text "Cannot unify types" <+> code s t1 <+> text "and" <+> code s t2
  header (InfiniteType v t) _ = text "Cannot construct an infinite type"
  header (NotInScope i) s = code s i <+> text "is not in scope"
  header (Ambigious cs) _ = text "Cannot match types"
  header (UnificationMismatch ts1 ts2) _ = text "Types does not match"

  details (UnificationFail t1 t2) _ = empty
  details (InfiniteType v t) s = code s v <+> equals <+> code s t
  details (NotInScope i) _ = empty
  details (Ambigious cs) s = vcat (map (formatConstraint s) cs)
    where formatConstraint s (t1, t2) = text "Expected" <+> code s t1 <+> text "but got" <+> code s t2
  details (UnificationMismatch ts1 ts2) s = vcat (zipWith (formatTypes s) ts1 ts2)
    where formatTypes s t1 t2 | t1 == t2 = code s t1 <+> text "==" <+> code s t2
          formatTypes s t1 t2 = code s t1 <+> text "!=" <+> code s t2


module Oden.Output.Infer where

import Text.PrettyPrint

import Oden.Output
import Oden.Infer
import Oden.Infer.Subsumption

instance OdenOutput TypeError where
  outputType _ = Error

  name (UnificationFail _ _)              = "Infer.UnificationFail"
  name (InfiniteType _ _)                 = "Infer.InfiniteType"
  name (NotInScope _)                     = "Infer.NotInScope"
  name (Ambigious _)                      = "Infer.Ambigious"
  name (UnificationMismatch _ _)          = "Infer.UnificationMismatch"
  name (ArgumentCountMismatch _ _)        = "Infer.ArgumentCountMismatch"
  name (TypeSignatureSubsumptionError _ (SubsumptionError _ _)) = "Infer.TypeSignatureSubsumptionError"

  header (UnificationFail t1 t2) s = text "Cannot unify types" <+> code s t1 <+> text "and" <+> code s t2
  header (InfiniteType _ _) _ = text "Cannot construct an infinite type"
  header (NotInScope i) s = code s i <+> text "is not in scope"
  header (Ambigious _) _ = text "Cannot match types"
  header (UnificationMismatch _ _) _ = text "Types do not match"
  header (ArgumentCountMismatch as ps) _ | length as > length ps =
    text "Function is applied to too few arguments"
  header (ArgumentCountMismatch _ _) _ =
    text "Function is applied to too many arguments"
  header (TypeSignatureSubsumptionError n SubsumptionError{}) s =
    text "Type signature for" <+> strCode s n
    <+> text "does not subsume the type of the definition"

  details (UnificationFail _ _) _ = empty
  details (InfiniteType v t) s = code s v <+> equals <+> code s t
  details (NotInScope _) _ = empty
  details (Ambigious cs) s = vcat (map formatConstraint cs)
    where formatConstraint (t1, t2) = text "Expected" <+> code s t1 <+> text "but got" <+> code s t2
  details (UnificationMismatch ts1 ts2) s = vcat (zipWith formatTypes ts1 ts2)
    where formatTypes t1 t2 | t1 == t2 = code s t1 <+> text "==" <+> code s t2
          formatTypes t1 t2 = code s t1 <+> text "!=" <+> code s t2
  details (ArgumentCountMismatch as1 as2) s =
    text "Expected:" <+> vcat (map (code s) as1)
    $+$ text "Actual:" <+> vcat (map (code s) as2)
  details (TypeSignatureSubsumptionError _ (SubsumptionError t1 t2)) s =
    text "Type" <+> code s t1 <+> text "does not subsume" <+> code s t2

  sourceInfo _ = Nothing

module Oden.Output.Unification where

import Text.PrettyPrint

import Oden.Infer.Unification
import Oden.Output
import Oden.Pretty

instance OdenOutput UnificationError where
  outputType _ = Error

  name UnificationFail{}         = "Infer.UnificationFail"
  name RowFieldUnificationFail{} = "Infer.UnificationFail"
  name InfiniteType{}            = "Infer.InfiniteType"
  name UnificationMismatch{}     = "Infer.UnificationMismatch"
  name ParameterCountMismatch{}   = "Infer.ArgumentCountMismatch"

  header (UnificationFail _ t1 t2) s = text "Cannot unify types"
    <+> code s (pp t1) <+> text "and" <+> code s (pp t2)
  header (RowFieldUnificationFail _ (l1, _) (l2, _)) s = text "Cannot unify fields"
    <+> code s (pp l1) <+> text "and" <+> code s (pp l2)
  header InfiniteType{} _ = text "Cannot construct an infinite type"
  header UnificationMismatch{} _ = text "Types do not match"
  header (ParameterCountMismatch _ _ _ actual expected) _ =
    text "Cannot apply a function of"
    <+> int (length actual)
    <+> pluralize "parameter" (length actual)
    <+> text "to"
    <+> int (length expected)
    <+> pluralize "argument" (length expected)

  details UnificationFail{} _ = empty
  details (RowFieldUnificationFail _ (l1, t1) (l2, t2)) s =
    text "The field" <+> code s (pp l1) <+> text "has type" <+> code s (pp t1)
    <+> text "and the field" <+> code s (pp l2) <+> text "has type" <+> code s (pp t2)
  details (InfiniteType _ v t) s = code s (pp v) <+> equals <+> code s (pp t)
  details (UnificationMismatch _ left right) s = formatTypes left right
    where formatTypes (t1:ts1) (t2:ts2)
            | t1 == t2  = code s (pp t1) <+> text "==" <+> code s (pp t2) $+$ formatTypes ts1 ts2
            | otherwise =  code s (pp t1) <+> text "!=" <+> code s (pp t2) $+$ formatTypes ts1 ts2
          formatTypes (t1:ts1) [] = code s (pp t1) <+> text "!= <none>" $+$ formatTypes ts1 []
          formatTypes [] (t2:ts2) = text "<none> !=" <+> code s (pp t2) $+$ formatTypes [] ts2
          formatTypes [] [] = empty
  details (ParameterCountMismatch _ expectedType actualType _ _) s =
    text "Expected function type:" <+> code s (pp expectedType)
    $+$ text "Actual function type:" <+> code s (pp actualType)

  sourceInfo (UnificationFail si _ _)            = Just si
  sourceInfo (RowFieldUnificationFail si _ _)    = Just si
  sourceInfo (InfiniteType si _ _)               = Just si
  sourceInfo (UnificationMismatch si _ _)        = Just si
  sourceInfo (ParameterCountMismatch si _ _ _ _) = Just si

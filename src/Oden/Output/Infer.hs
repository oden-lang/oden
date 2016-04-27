module Oden.Output.Infer where

import           Text.PrettyPrint.Leijen

import           Oden.Infer
import           Oden.Infer.Subsumption
import           Oden.Output
import           Oden.Output.Unification ()
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

instance OdenOutput TypeError where
  outputType _ = Error

  name (UnificationError e)                                 = name e
  name InvalidPackageReference{}                            = "Infer.InvalidPackageReference"
  name (NotInScope _ _)                                     = "Infer.NotInScope"
  name MemberNotInPackage{}                                 = "Infer.MemberNotInPackage"
  name PackageNotInScope{}                                  = "Infer.PackageNotInScope"
  name ArgumentCountMismatch{}                              = "Infer.ArgumentCountMismatch"
  name (TypeSignatureSubsumptionError _ SubsumptionError{}) = "Infer.TypeSignatureSubsumptionError"
  name ValueUsedAsType{}                                    = "Infer.ValueUsedAsType"
  name ProtocolUsedAsType{}                                 = "Infer.ProtocolUsedAsType"
  name NotAnExpression{}                                    = "Infer.NotAnExpression"
  name NotAProtocol{}                                       = "Infer.NotAProtocol"
  name NoSuchMethodInProtocol{}                             = "Infer.NoSuchMethodInProtocol"
  name InvalidForeignFnApplication{}                        = "Infer.InvalidForeignFnApplication"

  header (UnificationError e) s                             = header e s
  header (InvalidPackageReference _ p) s = text "Invalid reference to package" <+> code s (pretty p)
  header (NotInScope _ i) s = code s (pretty i) <+> text "is not in scope"
  header (PackageNotInScope _ p) s = text "Package" <+> code s (pretty p) <+> text "is not in scope"
  header (MemberNotInPackage _ p m) s = code s (pretty m) <+> text "is not a member of package" <+> code s (pretty p)
  header (ArgumentCountMismatch _ as ps) _ | length as > length ps =
    text "Function is applied to too few arguments"
  header ArgumentCountMismatch{} _ =
    text "Function is applied to too many arguments"
  header (TypeSignatureSubsumptionError n SubsumptionError{}) s =
    text "Type signature for" <+> code s (pretty n)
    <+> text "does not subsume the type of the definition"
  header (ValueUsedAsType _ n) s =
    text "The value" <+> code s (pretty n)
    <+> text "cannot be used as a type"
  header (ProtocolUsedAsType _ n) s =
    text "The protocol" <+> code s (pretty n)
    <+> text "cannot be used as a type"
  header (NotAnExpression _ n) s =
    text "The type" <+> code s (pretty n)
    <+> text "is not an expression"
  header (NotAProtocol _ n) s =
    code s (pretty n) <+> text "is not a protocol"
  header (NoSuchMethodInProtocol _ (Protocol _ protocolName _ _) n) s =
    code s (pretty n)
    <+> text "is not a method in"
    <+> code s (pretty protocolName)
  header (InvalidForeignFnApplication _) _ =
    text "Invalid foreign function application"

  details (UnificationError e) s = details e s
  details InvalidPackageReference{} _ = text "Packages cannot be referenced as values"
  details NotInScope{} _ = empty
  details PackageNotInScope{} _ = empty
  details MemberNotInPackage{} _ = empty
  details (ArgumentCountMismatch _ as1 as2) s = vcat [
      text "Expected:" <+> vcat (map (code s . pretty) as1),
      text "Actual:" <+> vcat (map (code s . pretty) as2)
    ]
    -- TODO: Print something like "In the expression: ..."
  details (TypeSignatureSubsumptionError _ (SubsumptionError _ t1 t2)) s =
    text "Type" <+> code s (pretty t1) <+> text "does not subsume" <+> code s (pretty t2)
  details ValueUsedAsType{} _ = empty
  details ProtocolUsedAsType{} _ = empty
  details NotAnExpression{} _ = empty
  details NotAProtocol{} _ = empty
  details NoSuchMethodInProtocol{} _ = empty
  details (InvalidForeignFnApplication _) _ =
    text $ unwords [
      "Foreign function application expressions are used by the compiler only",
      "and should not appear in the Untyped IR before type inference. If you",
      "get this error it's probably because of a programming error in the",
      "compiler itself, or a tool using the compiler."
    ]

  sourceInfo (UnificationError e)                                        = sourceInfo e
  sourceInfo (ArgumentCountMismatch e _ _)                               = Just (getSourceInfo e)
  sourceInfo (NotInScope si _)                                           = Just si
  sourceInfo (PackageNotInScope si _)                                    = Just si
  sourceInfo (MemberNotInPackage si _ _)                                 = Just si
  sourceInfo (InvalidPackageReference si _)                              = Just si
  sourceInfo (TypeSignatureSubsumptionError _ (SubsumptionError si _ _)) = Just si
  sourceInfo (ValueUsedAsType si _)                                      = Just si
  sourceInfo (ProtocolUsedAsType si _)                                   = Just si
  sourceInfo (NotAnExpression si _)                                      = Just si
  sourceInfo (NotAProtocol si _)                                         = Just si
  sourceInfo (NoSuchMethodInProtocol si _ _)                             = Just si
  sourceInfo (InvalidForeignFnApplication si)                            = Just si

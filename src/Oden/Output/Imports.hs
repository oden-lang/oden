{-# LANGUAGE LambdaCase #-}
module Oden.Output.Imports () where

import           Data.List
import           Text.PrettyPrint.Leijen

import           Oden.Imports
import           Oden.Output
import           Oden.Pretty             ()

instance OdenOutput PackageImportError where
  outputType _ = Error

  name =
    \case
      NativePackageNotFound{} ->
        "Imports.NativePackageNotFound"
      ForeignPackageImportError{} ->
        "Imports.PackageImportError"

  header err s =
    case err of
      NativePackageNotFound n ->
        text "Package not found:" <+> strCode s (intercalate "/" n)
      ForeignPackageImportError n _ ->
        text "Failed to import Go package:" <+> strCode s n

  details err _ =
    case err of
      NativePackageNotFound{} ->
        empty
      ForeignPackageImportError _ msg ->
        text msg

  sourceInfo =
    \case
      _ -> Nothing

instance OdenOutput UnsupportedTypesWarning where
  outputType _  = Warning
  name _        = "Imports.UnsupportedTypesWarning"
  header u s    = text "Some definitions could not be imported from package" <+> strCode s (intercalate "/" (pkg u)) <> colon
  details u s   = vcat (map formatMessage (messages u))
    where formatMessage (n, msg) = code s (pretty n)
                                   <+> parens (text msg <+> text "are not supported")
  sourceInfo _ = Nothing

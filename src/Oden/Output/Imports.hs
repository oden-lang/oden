{-# LANGUAGE LambdaCase #-}
module Oden.Output.Imports () where

import           Text.PrettyPrint.Leijen

import           Oden.Imports
import           Oden.QualifiedName      (PackageName(..))
import           Oden.Output
import           Oden.Pretty             ()
import           Oden.Output.Identifier  ()

instance OdenOutput PackageImportError where
  outputType _ = Error

  name =
    \case
      PackageNotFound{} ->
        "Imports.PackageNotFound"
      ForeignPackageImportError{} ->
        "Imports.PackageImportError"
      IllegalImportPackageName{} ->
        "Imports.IllegalImportPackageName"

  header err s =
    case err of
      PackageNotFound pkgName@NativePackageName{} ->
        text "Package not found:" <+> pretty pkgName
      PackageNotFound (ForeignPackageName pkgName) ->
        text "Foreign package not found:" <+> strCode s pkgName
      ForeignPackageImportError n _ ->
        text "Failed to import Go package:" <+> strCode s n
      IllegalImportPackageName _ nameErr ->
        header nameErr s

  details err s =
    case err of
      PackageNotFound{} ->
        empty
      ForeignPackageImportError _ msg ->
        text msg
      IllegalImportPackageName _ nameErr ->
        details nameErr s

  sourceInfo =
    \case
      _ -> Nothing

instance OdenOutput UnsupportedTypesWarning where
  outputType _ =
    Warning

  name _ =
    "Imports.UnsupportedTypesWarning"

  header u s =
    text "Some definitions could not be imported from package"
    <+> strCode s (pkg u) <> colon

  details u s =
    vcat (map formatMessage (messages u))
    where formatMessage (n, msg) =
            code s (pretty n)
            <+> parens (text msg <+> text "are not supported")

  sourceInfo _ =
    Nothing

{-# LANGUAGE LambdaCase #-}
module Oden.Infer.Environment (
  TypeBinding(..),
  TypingEnvironment,
  fromPackage,
  fromPackages
) where

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Package
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Typed

import           Oden.Environment                 hiding (map)
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName               (QualifiedName (..))
import           Oden.SourceInfo
import           Oden.Type.Polymorphic

data TypeBinding = PackageBinding (Metadata SourceInfo) Identifier TypingEnvironment
                 | Local (Metadata SourceInfo) Identifier Scheme
                 | Type (Metadata SourceInfo) QualifiedName [NameBinding] Type
                 | QuantifiedType (Metadata SourceInfo) Identifier Type
                 | ProtocolBinding (Metadata SourceInfo) Identifier Protocol
                 deriving (Show, Eq)

type TypingEnvironment = Environment TypeBinding (ProtocolImplementation TypedExpr)

fromPackage :: TypedPackage
            -> TypingEnvironment
fromPackage (TypedPackage _ _ defs) =
  foldl1 merge (map convert defs)
  where
  convert =
    \case
      Definition si n (sc, _)                     ->
        singleton n (Local si n sc)
      ForeignDefinition si n sc                   ->
        singleton n (Local si n sc)
      TypeDefinition si qualified@(FQN _ n) bs t  ->
        singleton n (Type si qualified bs t)
      ProtocolDefinition si (FQN _ name) protocol ->
        singleton name (ProtocolBinding si name protocol)
      Implementation _ impl ->
        singletonImplementation impl

fromPackages :: [ImportedPackage TypedPackage] -> TypingEnvironment
fromPackages =
  foldl iter empty
  where
  iter env (ImportedPackage importRef pkgIdentifier pkg) =
    env `extend` ( pkgIdentifier
                 , PackageBinding
                   (Metadata $ getSourceInfo importRef)
                   pkgIdentifier
                   (fromPackage pkg))

{-# LANGUAGE LambdaCase #-}
module Oden.Compiler.TypeEncoder (
    encodeTypeInstance,
    encodeMethodInstance
) where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List             (intercalate, intersperse, sortOn)

import           Oden.Identifier
import           Oden.QualifiedName    (PackageName (..), QualifiedName (..))
import           Oden.Type.Polymorphic

type Level = Int
type TypeEncoder t = StateT Level (Writer String) t

pad :: TypeEncoder ()
pad = do
  n <- get
  tell (replicate n '_')


withIncreasedLevel :: TypeEncoder () -> TypeEncoder ()
withIncreasedLevel e = do
  modify succ
  e
  modify pred


padded :: String -> TypeEncoder ()
padded s = pad >> tell s >> pad


paddedTo :: TypeEncoder ()
paddedTo = padded "to"


writeQualified :: QualifiedName -> TypeEncoder ()
writeQualified (FQN pkgName name) =
  case pkgName of
    NativePackageName [] ->
      tell (asString name)
    NativePackageName segments ->
      tell (intercalate "_" segments ++ "_" ++ asString name)
    ForeignPackageName foreignPkgName ->
      tell ("foreign_" ++ foreignPkgName ++ "_" ++ asString name)


writeType :: Type -> TypeEncoder ()
writeType (TVar _ (TV name)) = tell name
writeType (TCon _ n) = writeQualified n
writeType (TTuple _ f s r) = do
  tell "tupleof"
  pad
  foldl writeElement (return ()) (f:s:r)
  where
  writeElement a t = a >> withIncreasedLevel (writeType t) >> pad
writeType (TApp _ tf tp) = do
  withIncreasedLevel (writeType tf)
  padded "of"
  withIncreasedLevel (writeType tp)
writeType (TNoArgFn _ t') = do
  tell "to"
  pad
  withIncreasedLevel (writeType t')
writeType (TFn _ tl tr) = do
  withIncreasedLevel (writeType tl)
  paddedTo
  withIncreasedLevel (writeType tr)
writeType (TForeignFn si variadic ps rs) = do
  sequence_ (intersperse paddedTo (map (withIncreasedLevel . writeType) ps))
  when variadic $ do
    pad
    tell "variadic"
  paddedTo
  case rs of
    [] -> undefined
    [r] -> withIncreasedLevel (writeType r)
    (r1:r2:rt) -> withIncreasedLevel $ writeType (TTuple si r1 r2 rt)
writeType (TSlice _ t) = do
  tell "sliceof"
  pad
  withIncreasedLevel (writeType t)
writeType (TRecord _ row) = do
  tell "record"
  pad
  writeType row
writeType (TNamed _ n t) = do
  writeQualified n
  withIncreasedLevel (writeType t)
writeType REmpty{} = tell "emptyrow"
writeType row@RExtension{} = do
  tell "row"
  pad
  foldl writeField (return ()) (sortOn fst (rowToList row))
  where
  writeField a (identifier, t) = do
    _ <- a
    withIncreasedLevel $ do
      tell (asString identifier)
      pad
      writeType t
    pad
writeType (TConstrained _ t) = writeType t

encodeType :: Type -> String
encodeType t = snd (runWriter (runStateT (writeType t) 1))

encodeTypeInstance :: Identifier -> Type -> String
encodeTypeInstance identifier type' =
  asString identifier ++ "_inst_" ++ encodeType type'

encodeMethodInstance :: QualifiedName
                     -> Identifier
                     -> Type
                     -> String
encodeMethodInstance (FQN _ protocolName') methodName type' =
  asString protocolName' ++ "_method_" ++ asString methodName ++ "_inst_" ++ encodeType type'

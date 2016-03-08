module Oden.Compiler.TypeEncoder (
    encodeTypeInstance
) where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List             (intercalate)

import           Oden.Identifier
import           Oden.QualifiedName    (QualifiedName(..))
import qualified Oden.Type.Monomorphic as Mono
import           Oden.Type.Basic

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

writeBasic :: BasicType -> TypeEncoder ()
writeBasic TInt = tell "int"
writeBasic TBool = tell "bool"
writeBasic TString = tell "string"

writeType :: Mono.Type -> TypeEncoder ()
writeType (Mono.TAny _) = tell "any"
writeType (Mono.TUnit _) = tell "unit"
writeType (Mono.TBasic _ b) = writeBasic b
writeType (Mono.TCon _ d r) = do
  writeType d
  pad
  tell "of"
  pad
  writeType r
writeType (Mono.TTuple _ f s r) = do
  tell "tupleof"
  pad
  foldl writeElement (return ()) (f:s:r)
  where
  writeElement a t = a >> withIncreasedLevel (writeType t) >> pad
writeType (Mono.TNoArgFn _ t') = do
  tell "to"
  pad
  withIncreasedLevel (writeType t')
writeType (Mono.TFn _ tl tr) = do
  withIncreasedLevel (writeType tl)
  paddedTo
  withIncreasedLevel (writeType tr)
writeType (Mono.TUncurriedFn _ as r) = do
  foldl writeArg (return ()) as
  withIncreasedLevel (writeType r)
  where
  writeArg a t = a >> withIncreasedLevel (writeType t) >> paddedTo
writeType (Mono.TVariadicFn _ as v r) = do
  foldl writeArg (return ()) as
  tell "variadic"
  pad
  withIncreasedLevel (writeType v)
  paddedTo
  withIncreasedLevel (writeType r)
  where
  writeArg a t = a >> withIncreasedLevel (writeType t) >> paddedTo
writeType (Mono.TSlice _ t) = do
  tell "sliceof"
  pad
  withIncreasedLevel (writeType t)
writeType (Mono.TStruct _ fs) = do
  tell "struct"
  pad
  foldl writeField (return ()) fs
  where
  writeField a (Mono.TStructField _ identifier t) = do
    _ <- a
    withIncreasedLevel $ do
      tell (asString identifier)
      pad
      writeType t
    pad
writeType (Mono.TNamed _ (FQN ns name) t) = do
  let parts = (ns ++ [asString name]) :: [String]
  tell (intercalate "_" parts)
  withIncreasedLevel (writeType t)

writeTypeInstance :: Identifier -> Mono.Type -> TypeEncoder ()
writeTypeInstance identifier typeInstance = do
  tell (asString identifier)
  pad
  tell "inst"
  pad
  writeType typeInstance

encodeTypeInstance :: Identifier -> Mono.Type -> String
encodeTypeInstance i t =
  let (_, encoded) = runWriter (runStateT (writeTypeInstance i t) 1)
  in encoded

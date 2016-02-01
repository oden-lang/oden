module Oden.Compiler.TypeEncoder (
    encodeTypeInstance
) where

import Control.Monad.State
import Control.Monad.Writer

import Oden.Identifier
import qualified Oden.Type.Monomorphic as Mono

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

paddedTo :: TypeEncoder ()
paddedTo = pad >> tell "to" >> pad

writeType :: Mono.Type -> TypeEncoder ()
writeType Mono.TAny = tell "any"
writeType Mono.TUnit = tell "unit"
writeType (Mono.TCon s) = tell s
writeType (Mono.TNoArgFn t') = do
  tell "to"
  pad
  withIncreasedLevel (writeType t')
writeType (Mono.TFn tl tr) = do
  withIncreasedLevel (writeType tl)
  paddedTo
  withIncreasedLevel (writeType tr)
writeType (Mono.TUncurriedFn as r) = do
  foldl writeArg (return ()) as
  withIncreasedLevel (writeType r)
  where
  writeArg a t = a >> withIncreasedLevel (writeType t) >> paddedTo
writeType (Mono.TVariadicFn as v r) = do
  foldl writeArg (return ()) as
  tell "variadic"
  pad
  withIncreasedLevel (writeType v)
  paddedTo
  withIncreasedLevel (writeType r)
  where
  writeArg a t = a >> withIncreasedLevel (writeType t) >> paddedTo
writeType (Mono.TSlice t) = do
  tell "sliceof"
  pad
  withIncreasedLevel (writeType t)

writeTypeInstance :: Identifier -> Mono.Type -> TypeEncoder ()
writeTypeInstance i t = do
  tell (writeIdentifier i)
  pad
  tell "inst"
  pad
  writeType t
  where
  writeIdentifier (Unqualified n) = n
  writeIdentifier (Qualified p n) = p ++ "_" ++ n

encodeTypeInstance :: Identifier -> Mono.Type -> Name
encodeTypeInstance i t =
  let (_, encoded) = runWriter (runStateT (writeTypeInstance i t) 1)
  in encoded

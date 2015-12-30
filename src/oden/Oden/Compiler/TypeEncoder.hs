module Oden.Compiler.TypeEncoder (
    encodeTypeInstance
) where

import Control.Monad.State
import Control.Monad.Writer
import Data.List (intercalate)

import Oden.Identifier
import qualified Oden.Core as Core
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

writeType :: Mono.Type -> TypeEncoder ()
writeType (Mono.TCon s) = tell s
writeType (Mono.TArrSingle t') = do
  tell "to"
  pad
  withIncreasedLevel (writeType t')
writeType (Mono.TArr tl tr) = do
  withIncreasedLevel (writeType tl)
  pad
  tell "to"
  pad
  withIncreasedLevel (writeType tr)

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

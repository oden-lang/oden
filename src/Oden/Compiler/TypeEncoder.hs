module Oden.Compiler.TypeEncoder (
    encodeTypeInstance,
    encodeMethodInstance
) where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List             (intercalate, intersperse, sortOn)

import           Oden.Identifier
import           Oden.QualifiedName    (QualifiedName(..))
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

padded :: String -> TypeEncoder ()
padded s = pad >> tell s >> pad

paddedTo :: TypeEncoder ()
paddedTo = padded "to"

writeQualified :: QualifiedName -> TypeEncoder ()
writeQualified (FQN pkgs name) = do
  let parts = (pkgs ++ [asString name]) :: [String]
  tell (intercalate "_" parts)

writeType :: Mono.Type -> TypeEncoder ()
writeType (Mono.TCon _ n) = writeQualified n
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
writeType (Mono.TForeignFn si variadic ps rs) = do
  sequence_ (intersperse paddedTo (map (withIncreasedLevel . writeType) ps))
  when variadic $ do
    pad
    tell "variadic"
  paddedTo
  case rs of
    [] -> undefined
    [r] -> withIncreasedLevel (writeType r)
    (r1:r2:rt) -> withIncreasedLevel $ writeType (Mono.TTuple si r1 r2 rt)
writeType (Mono.TSlice _ t) = do
  tell "sliceof"
  pad
  withIncreasedLevel (writeType t)
writeType (Mono.TRecord _ row) = do
  tell "record"
  pad
  writeType row
writeType (Mono.TNamed _ n t) = do
  writeQualified n
  withIncreasedLevel (writeType t)
writeType Mono.REmpty{} = tell "emptyrow"
writeType row@Mono.RExtension{} = do
  tell "row"
  pad
  foldl writeField (return ()) (sortOn fst (Mono.rowToList row))
  where
  writeField a (identifier, t) = do
    _ <- a
    withIncreasedLevel $ do
      tell (asString identifier)
      pad
      writeType t
    pad

encodeType :: Mono.Type -> String
encodeType t = snd (runWriter (runStateT (writeType t) 1))

encodeTypeInstance :: Identifier -> Mono.Type -> String
encodeTypeInstance identifier type' =
  asString identifier ++ "_inst_" ++ encodeType type'

encodeMethodInstance :: QualifiedName
                     -> Identifier
                     -> Mono.Type
                     -> String
encodeMethodInstance (FQN _ protocolName) methodName type' =
  asString protocolName ++ "_method_" ++ asString methodName ++ "_inst_" ++ encodeType type'

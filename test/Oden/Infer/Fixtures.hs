module Oden.Infer.Fixtures where

import           Oden.SourceInfo
import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.Predefined
import           Oden.Type.Polymorphic

missing :: Metadata SourceInfo
missing = Metadata Missing

predefined :: Metadata SourceInfo
predefined = Metadata Predefined

tvA = TV "a"
tvB = TV "b"
tvC = TV "c"

tvarA :: Type
tvarA = TVar predefined tvA

tvarB :: Type
tvarB = TVar predefined tvB

tvarC :: Type
tvarC = TVar predefined tvC

typeAny = TAny missing

scheme:: Type -> Scheme
scheme= Forall (Metadata Predefined) []

typeFn :: Type -> Type -> Type
typeFn = TFn missing

named :: String -> Type -> Type
named = TNamed missing . FQN [] . Identifier

typeSlice :: Type -> Type
typeSlice = TSlice missing

intSlice = typeSlice typeInt

typeNoArgFn = TNoArgFn missing
typeUncurried = TUncurriedFn missing
typeVariadic = TVariadicFn missing

emptyRow = REmpty missing

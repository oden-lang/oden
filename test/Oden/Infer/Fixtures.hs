module Oden.Infer.Fixtures where

import           Oden.Core.Typed        as Typed
import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Operator
import           Oden.Core.Untyped      hiding (Definition (..))
import qualified Oden.Core.Untyped      as Untyped
import           Oden.Environment       hiding (map)
import           Oden.Identifier
import           Oden.Infer.Environment
import           Oden.Metadata
import           Oden.Predefined
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic
import           Oden.Type.Signature

import qualified Data.Set as Set

missing :: Metadata SourceInfo
missing = Metadata Missing

predefined :: Metadata SourceInfo
predefined = Metadata Predefined

tvA = TV "a"
tvB = TV "b"
tvC = TV "c"
tvD = TV "d"

tvarA = TVar predefined tvA
tvarB = TVar predefined tvB
tvarC = TVar predefined tvC
tvarD = TVar predefined tvD

scheme:: Type -> Scheme
scheme t = Forall predefined (map (TVarBinding missing) $ Set.toList (ftv t)) Set.empty t

typeFn :: Type -> Type -> Type
typeFn = TFn missing

named :: String -> Type -> Type
named = TNamed missing . FQN [] . Identifier

typeSlice :: Type -> Type
typeSlice = TSlice missing

intSlice = typeSlice typeInt

typeNoArgFn = TNoArgFn missing
typeForeign = TForeignFn missing

typeRecord = TRecord missing
emptyRow = REmpty missing
rowExt = RExtension missing

tsUnit = TSUnit Missing
tsVar = TSSymbol Missing . Identifier
tsSymbol = TSSymbol Missing
tsFn = TSFn Missing
tsRecord = TSRecord Missing
tsRowEmpty = TSRowEmpty Missing
tsRowExt = TSRowExtension Missing

implicit = TypeSignature Missing []
explicit = TypeSignature Missing
varBinding = SignatureVarBinding Missing . Identifier

forall = Forall missing
tvarBinding = TVarBinding missing

untyped :: (Untyped -> UntypedExpr) -> UntypedExpr
untyped = flip ($) Untyped

tSymbol                 = Symbol missing
tOp                     = BinaryOp missing
tApplication            = Application missing
tNoArgApplication       = NoArgApplication missing
tForeignFnApplication   = ForeignFnApplication missing
tFn                     = Fn missing
tNoArgFn                = NoArgFn missing
tLet                    = Let missing
tLiteral                = Literal missing
tTuple                  = Tuple missing
tIf                     = If missing
tSlice                  = Slice missing
tBlock                  = Block missing
tFieldAccess            = MemberAccess missing . RecordFieldAccess
tPackageMemberAcccess   = MemberAccess missing . PackageMemberAccess

tUnit   = Unit
tInt    = Int
tString = String
tBool   = Bool

tDefinition = Definition missing
tNameBinding = NameBinding missing

predef :: TypingEnvironment
predef = fromPackage universe

predefAndStringLength :: TypingEnvironment
predefAndStringLength =  predef `extend` (Identifier "stringLength",
                                          Local predefined (Identifier "stringLength") $ scheme (typeFn typeString typeInt))

predefAndMax :: TypingEnvironment
predefAndMax =  predef `extend` (Identifier "max",
                                 Local predefined (Identifier "max") $ scheme (typeForeign False [typeInt, typeInt] [typeInt]))

predefAndMaxVariadic :: TypingEnvironment
predefAndMaxVariadic = predef `extend` (Identifier "max",
                                        Local predefined (Identifier "max") $ scheme (typeForeign True [TSlice missing typeInt] [typeInt]))
testableProtocolMethod =
  ProtocolMethod
  predefined
  (Identifier "test")
  (Forall predefined [] Set.empty (TFn predefined tvarA typeBool))

testableProtocol =
  Protocol
  predefined
  (FQN [] (Identifier "Testable"))
  (TVar predefined tvA)
  [testableProtocolMethod]

predefAndTestableProtocol :: TypingEnvironment
predefAndTestableProtocol =
  predef
  `extend`
  (Identifier "Testable",
   ProtocolBinding predefined (Identifier "Testable") testableProtocol)

fooBarPkgEnv :: TypingEnvironment
fooBarPkgEnv = predef `extend` (Identifier "foo",
                                PackageBinding
                                missing
                                (Identifier "foo")
                                (fromList [(Identifier "Bar",
                                            Local predefined (Identifier "Bar") $ scheme typeInt)]))

booleanOp :: Type
booleanOp = typeFn typeBool (typeFn typeBool typeBool)

countToZero :: UntypedExpr
countToZero =
  Fn missing
  (NameBinding missing (Identifier "x"))
  (If missing
   (BinaryOp missing
    Equals
    (Symbol missing (Identifier "x") Untyped)
    (Literal missing (Int 0) Untyped)
    Untyped)
   (Literal missing (Int 0) Untyped)
   (Application missing
    (Symbol missing (Identifier "f") Untyped)
     (BinaryOp missing
      Subtract
      (Symbol missing (Identifier "x") Untyped)
      (Literal missing (Int 1) Untyped)
      Untyped)
     Untyped)
   Untyped)
  Untyped

intToInt :: Type
intToInt = typeFn typeInt typeInt

intToIntToInt :: Type
intToIntToInt = typeFn typeInt (typeFn typeInt typeInt)

countToZeroTyped :: Typed.TypedDefinition
countToZeroTyped =
  tDefinition
   (Identifier "f")
   (scheme (typeFn typeInt typeInt),
    tFn
    (tNameBinding (Identifier "x"))
    (tIf
     (tOp
      Equals
      (tSymbol (Identifier "x") typeInt)
      (tLiteral (tInt 0) typeInt)
      typeBool)
     (tLiteral (tInt 0) typeInt)
     (tApplication
      (tSymbol (Identifier "f") intToInt)
      (tOp
       Subtract
       (tSymbol (Identifier "x") typeInt)
       (tLiteral (tInt 1) typeInt)
       typeInt)
      typeInt)
     typeInt)
    intToInt)

twiceUntyped :: UntypedExpr
twiceUntyped =
  Fn missing
  (NameBinding missing (Identifier "f"))
  (Fn missing
   (NameBinding missing (Identifier "x"))
   (Application missing
     (Symbol missing (Identifier "f") Untyped)
     (Application missing
      (Symbol missing (Identifier "f") Untyped)
      (Symbol missing (Identifier "x") Untyped)
      Untyped)
     Untyped)
   Untyped)
  Untyped

twiceTyped :: Typed.TypedDefinition
twiceTyped =
  tDefinition (Identifier "twice") (scheme (typeFn (typeFn tvarA tvarA) (typeFn tvarA tvarA)),
                           tFn
                           (tNameBinding (Identifier "f"))
                           (tFn
                           (tNameBinding (Identifier "x"))
                           (tApplication
                             (tSymbol (Identifier "f") (typeFn tvarA tvarA))
                             (tApplication
                             (tSymbol (Identifier "f") (typeFn tvarA tvarA))
                             (tSymbol (Identifier "x") tvarA)
                             tvarA)
                             tvarA)
                           (typeFn tvarA tvarA))
                           (typeFn (typeFn tvarA tvarA) (typeFn tvarA tvarA)))

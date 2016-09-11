module Oden.Infer.Fixtures where

import           Oden.Core.Definition
import           Oden.Core.Expr
import           Oden.Core.Foreign
import           Oden.Core.ProtocolImplementation
import           Oden.Core.Typed                  as Typed
import           Oden.Core.Untyped                hiding (Definition (..),
                                                   MethodImplementation (..))

import           Oden.Environment                 hiding (map)
import           Oden.Identifier
import           Oden.Infer.Environment
import           Oden.Metadata
import           Oden.Predefined
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Polymorphic
import           Oden.Type.Signature

import qualified Data.Set                         as Set

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

channelOf :: Type -> Type
channelOf = TApp missing (TCon missing (nameInUniverse "Channel"))

receiverOf :: Type -> Type
receiverOf = TApp missing (TCon missing (nameInUniverse "Receiver"))

senderOf :: Type -> Type
senderOf = TApp missing (TCon missing (nameInUniverse "Sender"))

constrainedScheme :: Set.Set ProtocolConstraint -> Type -> Scheme
constrainedScheme constraints t = Forall predefined (map (TVarBinding missing) $ Set.toList (ftv t)) constraints t

scheme :: Type -> Scheme
scheme = constrainedScheme Set.empty

typeFn :: Type -> Type -> Type
typeFn = TFn missing

named :: String -> Type -> Type
named = TNamed missing . nameInUniverse

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

intToInt :: Type
intToInt = typeFn typeInt typeInt

intToIntToInt :: Type
intToIntToInt = typeFn typeInt (typeFn typeInt typeInt)

intToIntToBool :: Type
intToIntToBool = typeFn typeInt (typeFn typeInt typeBool)

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

testableProtocolName = nameInUniverse "Testable"
testableMethodName = Identifier "test"

testableProtocol =
  Protocol
  predefined
  testableProtocolName
  (TVar predefined tvA)
  [testableProtocolMethod]

testableProtocolMethod =
  ProtocolMethod
  predefined
  testableMethodName
  (Forall predefined [] Set.empty (TFn predefined tvarA typeBool))

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
   (Application
    missing
    (Application
     missing
     (MethodReference missing (NamedMethodReference (Identifier "Equality") (Identifier "EqualTo")) Untyped)
     (Symbol missing (Identifier "x") Untyped)
     Untyped)
    (Literal missing (Int 0) Untyped)
    Untyped)
   (Literal missing (Int 0) Untyped)
   (Application missing
    (Symbol missing (Identifier "f") Untyped)
     (Application
      missing
      (Application
       missing
       (MethodReference missing (NamedMethodReference (Identifier "Num") (Identifier "Subtract")) Untyped)
       (Symbol missing (Identifier "x") Untyped)
       Untyped)
      (Literal missing (Int 1) Untyped)
      Untyped)
     Untyped)
   Untyped)
  Untyped

equalsImplInt :: MethodImplementation TypedExpr
equalsImplInt =
  MethodImplementation
  missing
  (Identifier "EqualTo")
  (Foreign missing
    (ForeignBinaryOperator Equals)
    (typeFn typeInt (typeFn typeInt typeBool)))

subtractImplInt :: MethodImplementation TypedExpr
subtractImplInt =
  MethodImplementation
  missing
  (Identifier "Subtract")
  (Foreign missing (ForeignBinaryOperator Subtract) intToIntToInt)

countToZeroTyped :: Typed.TypedDefinition
countToZeroTyped =
  let equalityConstraint = ProtocolConstraint
                           missing
                           (nameInUniverse "Equality")
                           typeInt
      subtractionConstraint = ProtocolConstraint
                              missing
                              (nameInUniverse "Num")
                              typeInt
  in
  tDefinition
   (nameInUniverse "f")
   (constrainedScheme (Set.fromList []) (typeFn typeInt typeInt),
    tFn
    (tNameBinding (Identifier "x"))
    (tIf
     (tApplication
      (tApplication
       (MethodReference missing
        (Unresolved (nameInUniverse "Equality") (Identifier "EqualTo") equalityConstraint)
        (TConstrained (Set.singleton equalityConstraint) intToIntToBool))
       (tSymbol (Identifier "x") typeInt)
       (typeFn typeInt typeBool))
      (tLiteral (tInt 0) typeInt)
      typeBool)
     (tLiteral (tInt 0) typeInt)
     (tApplication
      (tSymbol (Identifier "f") intToInt)
      (tApplication
       (tApplication
        (MethodReference missing
         (Unresolved (nameInUniverse "Num") (Identifier "Subtract") subtractionConstraint)
         (TConstrained (Set.singleton subtractionConstraint) intToIntToInt))
        (tSymbol (Identifier "x") typeInt)
        intToInt)
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
  tDefinition
  (nameInUniverse "twice")
  (scheme (typeFn (typeFn tvarA tvarA) (typeFn tvarA tvarA)),
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

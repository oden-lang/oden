-- | This module contains values modelling the type signatures you _write_ in
-- Oden source code, not the actual type expressions get in the compiler. For
-- example, a 'TSSymbol' expression may refer to a type not in scope which
-- causes a compile time error.
module Oden.Type.Signature where

import Oden.Identifier
import Oden.SourceInfo

-- | The name and type pair for a struct field.
data TSStructField = TSStructField SourceInfo Identifier SignatureExpr
                   deriving (Show, Eq, Ord)

-- | A type expression used in type signatures and type annotations.
data SignatureExpr = TSUnit SourceInfo
                   | TSVar SourceInfo String
                   | TSSymbol SourceInfo Identifier
                   | TSApp SourceInfo SignatureExpr SignatureExpr
                   | TSFn SourceInfo SignatureExpr SignatureExpr
                   | TSNoArgFn SourceInfo SignatureExpr
                   | TSTuple SourceInfo SignatureExpr SignatureExpr [SignatureExpr]
                   | TSSlice SourceInfo SignatureExpr
                   | TSStruct SourceInfo [TSStructField]
                   deriving (Show, Eq, Ord)

-- | A type variable binding in an explicit quantification.
data SignatureVarBinding = SignatureVarBinding SourceInfo String
                         deriving (Show, Eq, Ord)

-- | A top level type signature with explicit or implicit type variable
-- quantification.
data TypeSignature = Explicit SourceInfo [SignatureVarBinding] SignatureExpr
                   | Implicit SourceInfo SignatureExpr
                   deriving (Show, Eq, Ord)

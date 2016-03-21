-- | This module contains values modelling the type signatures you _write_ in
-- Oden source code, not the actual type expressions get in the compiler. For
-- example, a 'TSSymbol' expression may refer to a type not in scope which
-- causes a compile time error.
--
-- All values are paramterized to support signatures where the source info is
-- considered metadata, i.e. not used when comparing with Eq.
module Oden.Type.Signature where

import Oden.Identifier

-- | A type expression used in type signatures and type annotations.
data SignatureExpr s = TSUnit s
                     | TSSymbol s Identifier
                     | TSApp s (SignatureExpr s) (SignatureExpr s)
                     | TSFn s (SignatureExpr s) (SignatureExpr s)
                     | TSNoArgFn s (SignatureExpr s)
                     | TSTuple s (SignatureExpr s) (SignatureExpr s) [SignatureExpr s]
                     | TSSlice s (SignatureExpr s)
                     | TSRowEmpty s
                     | TSRowExtension s Identifier (SignatureExpr s) (SignatureExpr s)
                     | TSRecord s (SignatureExpr s)
                     deriving (Show, Eq, Ord)

-- | A type variable binding in an explicit quantification.
data SignatureVarBinding s = SignatureVarBinding s Identifier
                           deriving (Show, Eq, Ord)

-- | A top level type signature with explicit or implicit type variable
-- quantification.
data TypeSignature s = TypeSignature s [SignatureVarBinding s] (SignatureExpr s)
                     deriving (Show, Eq, Ord)

module Oden.Type.Signature where

import Oden.Identifier
import Oden.SourceInfo

data SignatureExpr = TSUnit SourceInfo
                   | TSVar SourceInfo String
                   | TSSymbol SourceInfo Identifier
                   | TSApp SourceInfo SignatureExpr SignatureExpr
                   | TSFn SourceInfo SignatureExpr SignatureExpr
                   | TSNoArgFn SourceInfo SignatureExpr
                   | TSTuple SourceInfo SignatureExpr SignatureExpr [SignatureExpr]
                   | TSSlice SourceInfo SignatureExpr
                   deriving (Show, Eq, Ord)

data SignatureVarBinding = SignatureVarBinding SourceInfo String
                         deriving (Show, Eq, Ord)

data TypeSignature = Explicit SourceInfo [SignatureVarBinding] SignatureExpr
                   | Implicit SourceInfo SignatureExpr
                   deriving (Show, Eq, Ord)

-- | This module contains values modelling the type signatures you _write_ in
-- Oden source code, not the actual type expressions get in the compiler. For
-- example, a 'TSSymbol' expression may refer to a type not in scope which
-- causes a compile time error.
--
-- All values are paramterized to support signatures where the source info is
-- considered metadata, i.e. not used when comparing with Eq.
{-# LANGUAGE LambdaCase #-}
module Oden.Type.Signature where

import Oden.Identifier
import Oden.SourceInfo

-- | A type expression used in type signatures and type annotations.
data SignatureExpr = TSUnit SourceInfo
                   | TSSymbol SourceInfo Identifier
                   | TSApp SourceInfo SignatureExpr SignatureExpr
                   | TSFn SourceInfo SignatureExpr SignatureExpr
                   | TSNoArgFn SourceInfo SignatureExpr
                   | TSTuple SourceInfo SignatureExpr SignatureExpr [SignatureExpr]
                   | TSSlice SourceInfo SignatureExpr
                   | TSRowEmpty SourceInfo
                   | TSRowExtension SourceInfo Identifier SignatureExpr SignatureExpr
                   | TSRecord SourceInfo SignatureExpr
                   deriving (Show, Eq, Ord)

instance HasSourceInfo SignatureExpr where
  getSourceInfo =
    \case
      TSUnit si               -> si
      TSSymbol si _           -> si
      TSApp si _ _            -> si
      TSFn si _ _             -> si
      TSNoArgFn si _          -> si
      TSTuple si _ _ _        -> si
      TSSlice si _            -> si
      TSRowEmpty si           -> si
      TSRowExtension si _ _ _ -> si
      TSRecord si _           -> si
  setSourceInfo si =
    \case
      TSUnit _                            -> TSUnit si
      TSSymbol _ s                        -> TSSymbol si s
      TSApp _ cons param                  -> TSApp si cons param
      TSFn _ d r                          -> TSFn si d r
      TSNoArgFn _ r                       -> TSNoArgFn si r
      TSTuple _ f s r                     -> TSTuple si f s r
      TSSlice _ t                         -> TSSlice si t
      TSRowEmpty _                        -> TSRowEmpty si
      TSRowExtension _ label type' record -> TSRowExtension si label type' record
      TSRecord _ t                        -> TSRecord si t

-- | A type variable binding in an explicit quantification.
data SignatureVarBinding = SignatureVarBinding SourceInfo Identifier
                           deriving (Show, Eq, Ord)

-- | A top level type signature with explicit or implicit type variable
-- quantification.
data TypeSignature = TypeSignature SourceInfo [SignatureVarBinding] SignatureExpr
                     deriving (Show, Eq, Ord)

data ProtocolMethodSignature = ProtocolMethodSignature SourceInfo Identifier TypeSignature
                               deriving (Show, Eq, Ord)

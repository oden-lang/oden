{-# LANGUAGE LambdaCase #-}
module Oden.Compiler.LiteralEval where

import           Oden.Core.Expr
import           Oden.Core.Typed

import           Oden.Type.Polymorphic

import           Oden.Identifier
import           Oden.Predefined
import           Oden.QualifiedName

evaluateBinaryMethodApplication :: TypedMethodReference
                                -> TypedExpr
                                -> TypedExpr
                                -> Type
                                -> Maybe Literal
evaluateBinaryMethodApplication  f e1 e2 t =
  case f of
    Unresolved (FQN (NativePackageName []) (Identifier protocolName')) (Identifier method) _ ->
      case protocolName' of

        "Num" | t == typeInt -> do
                  (Int n1) <- evaluate e1
                  (Int n2) <- evaluate e2
                  case method of
                    "Add"      -> return $ Int (n1 + n2)
                    "Subtract" -> return $ Int (n1 - n2)
                    "Multiply" -> return $ Int (n1 * n2)
                    "Divide"   -> return $ Int (n1 `div` n2)
                    _          -> Nothing

        "Ordered" | typeOf e1 == typeInt -> do
                      (Int n1) <- evaluate e1
                      (Int n2) <- evaluate e2
                      case method of
                        "LessThan"         -> return $ Bool (n1 < n2)
                        "LessThanEqual"    -> return $ Bool (n1 <= n2)
                        "GreaterThan"      -> return $ Bool (n1 > n2)
                        "GreaterThanEqual" -> return $ Bool (n1 >= n2)
                        _                  -> Nothing

        "Logical" | t == typeBool -> do
                      (Bool b1) <- evaluate e1
                      (Bool b2) <- evaluate e2
                      case method of
                        "Conjunction" -> return $ Bool (b1 && b2)
                        "Disjunction" -> return $ Bool (b1 || b2)
                        _             -> Nothing

        "Equality" | typeOf e1 == typeInt -> do
                       (Int n1) <- evaluate e1
                       (Int n2) <- evaluate e2
                       case method of
                         "EqualTo"    -> return $ Bool (n1 == n2)
                         "NotEqualTo" -> return $ Bool (n1 /= n2)
                         _            -> Nothing
        "Equality" | typeOf e1 == typeBool -> do
                       (Bool b1) <- evaluate e1
                       (Bool b2) <- evaluate e2
                       case method of
                         "EqualTo"    -> return $ Bool (b1 == b2)
                         "NotEqualTo" -> return $ Bool (b1 /= b2)
                         _            -> Nothing
        _ -> Nothing
    _ -> Nothing

evaluate :: TypedExpr -> Maybe Literal
evaluate =
  \case
    Symbol{} -> Nothing

    Subscript{} -> Nothing
    Subslice{} -> Nothing

    Application _ (Application _ (MethodReference _ ref _) e1 _) e2 t ->
      evaluateBinaryMethodApplication ref e1 e2 t
    Application{} -> Nothing
    NoArgApplication{} -> Nothing
    ForeignFnApplication{} -> Nothing

    Fn{} -> Nothing
    NoArgFn{} -> Nothing
    Let{} -> Nothing

    RecordInitializer{} -> Nothing
    MemberAccess{} -> Nothing
    MethodReference{} -> Nothing

    (Literal _ l _) -> Just l

    (If _ p e1 e2 _) -> do
      (Bool b) <- evaluate p
      if b then evaluate e1
          else evaluate e2

    Slice{} -> Nothing
    Tuple{} -> Nothing
    Block{} -> Nothing
    Foreign{} -> Nothing
    Go{} -> Nothing
    Receive{} -> Nothing

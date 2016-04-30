module Oden.Compiler.LiteralEval where

import           Oden.Core.Expr
import           Oden.Core.Operator

evaluate :: Expr r t a -> Maybe Literal
evaluate Symbol{} = Nothing

evaluate Subscript{} = Nothing
evaluate Subslice{} = Nothing

evaluate (UnaryOp _ Positive e _) = evaluate e
evaluate (UnaryOp _ Negative e _) = do
  (Int n) <- evaluate e
  return $ Int (- n)
evaluate (UnaryOp _ Not e _) = do
  (Bool b) <- evaluate e
  return $ Bool (not b)

evaluate e@(BinaryOp _ Add _ _ _ ) = evaluateBinaryIntExpr e
evaluate e@(BinaryOp _ Subtract _ _ _) = evaluateBinaryIntExpr e
evaluate e@(BinaryOp _ Multiply _ _ _) = evaluateBinaryIntExpr e
evaluate e@(BinaryOp _ Divide _ _ _) = evaluateBinaryIntExpr e

evaluate (BinaryOp _ Equals e1 e2 _) = do
  v1 <- evaluate e1
  v2 <- evaluate e2
  return $ Bool (v1 == v2)

evaluate (BinaryOp _ Concat e1 e2 _) = do
  (String s1) <- evaluate e1
  (String s2) <- evaluate e2
  return (String (s1 ++ s2))

evaluate e@(BinaryOp _ LessThan _ _ _) = evaluateIntComparison e
evaluate e@(BinaryOp _ GreaterThan _ _ _) = evaluateIntComparison e
evaluate e@(BinaryOp _ LessThanEqual _ _ _) = evaluateIntComparison e
evaluate e@(BinaryOp _ GreaterThanEqual _ _ _) = evaluateIntComparison e

evaluate (BinaryOp _ And e1 e2 _) = do
  (Bool b1) <- evaluate e1
  (Bool b2) <- evaluate e2
  return $ Bool (b1 && b2)
evaluate (BinaryOp _ Or e1 e2 _) = do
  (Bool b1) <- evaluate e1
  (Bool b2) <- evaluate e2
  return $ Bool (b1 || b2)

evaluate Application{} = Nothing
evaluate NoArgApplication{} = Nothing
evaluate ForeignFnApplication{} = Nothing

evaluate Fn{} = Nothing
evaluate NoArgFn{} = Nothing
evaluate Let{} = Nothing

evaluate RecordInitializer{} = Nothing
evaluate MemberAccess{} = Nothing
evaluate MethodReference{} = Nothing

evaluate (Literal _ l _) = Just l

evaluate (If _ p e1 e2 _) = do
  (Bool b) <- evaluate p
  if b then evaluate e1
       else evaluate e2

evaluate Slice{} = Nothing
evaluate Tuple{} = Nothing
evaluate Block{} = Nothing

evaluateBinaryIntExpr :: Expr r t a -> Maybe Literal
evaluateBinaryIntExpr (BinaryOp _ op e1 e2 _) = do
  (Int n1) <- evaluate e1
  (Int n2) <- evaluate e2
  case op of
    Add -> return $ Int (n1 + n2)
    Subtract -> return $ Int (n1 - n2)
    Multiply -> return $ Int (n1 * n2)
    Divide -> return $ Int (n1 `div` n2)
    _      -> Nothing
evaluateBinaryIntExpr _ = Nothing

evaluateIntComparison :: Expr r t a -> Maybe Literal
evaluateIntComparison (BinaryOp _ op e1 e2 _) = do
  (Int n1) <- evaluate e1
  (Int n2) <- evaluate e2
  case op of
    LessThan -> return $ Bool (n1 < n2)
    GreaterThan -> return $ Bool (n1 > n2)
    LessThanEqual -> return $ Bool (n1 <= n2)
    GreaterThanEqual -> return $ Bool (n1 >= n2)
    _ -> Nothing
evaluateIntComparison _ = Nothing

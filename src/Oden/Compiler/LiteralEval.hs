module Oden.Compiler.LiteralEval where

import Oden.Core
import Oden.Core.Operator

evaluate :: Expr t -> Maybe Literal
evaluate Symbol{} = Nothing

-- Should probably be possible to evalute literal
-- expressions with literal slices and subslices as well.
evaluate Subscript{} = Nothing
evaluate Subslice{} = Nothing

-- We only do literal integer operations for now
evaluate (UnaryOp _ Positive e _) = evaluate e
evaluate (UnaryOp _ Negative e _) = do
  (Int n) <- evaluate e
  return $ Int (- n)
evaluate (UnaryOp _ Not e _) = Nothing

evaluate e@(BinaryOp _ Add _ _ _ ) = evaluateBinaryIntExpr e
evaluate e@(BinaryOp _ Subtract _ _ _) = evaluateBinaryIntExpr e
evaluate e@(BinaryOp _ Multiply _ _ _) = evaluateBinaryIntExpr e
evaluate e@(BinaryOp _ Divide _ _ _) = evaluateBinaryIntExpr e
evaluate BinaryOp{} = Nothing

evaluate Application{} = Nothing
evaluate NoArgApplication{} = Nothing
evaluate ForeignFnApplication{} = Nothing

evaluate Fn{} = Nothing
evaluate NoArgFn{} = Nothing
evaluate Let{} = Nothing

evaluate RecordInitializer{} = Nothing
evaluate RecordFieldAccess{} = Nothing
evaluate PackageMemberAccess{} = Nothing

evaluate (Literal _ l _) = Just l

evaluate If{} = Nothing
evaluate Slice{} = Nothing
evaluate Tuple{} = Nothing
evaluate Block{} = Nothing

evaluateBinaryIntExpr :: Expr t -> Maybe Literal
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

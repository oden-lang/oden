module Oden.Compiler.LiteralEval where

import           Oden.Core.Expr

evaluate :: Expr r t a -> Maybe Literal
evaluate Symbol{} = Nothing

evaluate Subscript{} = Nothing
evaluate Subslice{} = Nothing

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
evaluate Foreign{} = Nothing

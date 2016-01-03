module Oden.Eval where

import           Oden.Core.Untyped
import           Oden.Identifier

import           Control.Monad.Identity
import qualified Data.Map               as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr TermEnv
  | VNoArgClosure Expr TermEnv

type TermEnv = Map.Map Identifier Value
type Interpreter t = Identity t

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show (VInt n) = show n
  show (VBool n) = show n
  show VClosure{} = "<<closure>>"

eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
  Literal (Int k)  -> return $ VInt k
  Literal (Bool k) -> return $ VBool k

  Symbol x -> do
    let Just v = Map.lookup x env
    return v

  Fn x body ->
    return (VClosure x body env)

  NoArgFn body ->
    return (VNoArgClosure body env)

  Application fun arg -> do
    VClosure x body clo <- eval env fun
    argv <- eval env arg
    let nenv = Map.insert (Unqualified x) argv clo
    eval nenv body

  NoArgApplication fun -> do
    VNoArgClosure body clo <- eval env fun
    eval clo body

  Let x e body -> do
    e' <- eval env e
    let nenv = Map.insert (Unqualified x) e' env
    eval nenv body

  If cond tr fl -> do
    VBool br <- eval env cond
    if br
    then eval env tr
    else eval env fl

runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  let res = runIdentity (eval env ex) in
  (res, Map.insert (Unqualified nm) res env)

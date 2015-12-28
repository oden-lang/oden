module Oden.Predefined (
  predefined
) where

import Oden.Identifier
import Oden.Type.Polymorphic
import qualified Oden.Env as Env

predefined :: Env.Env
predefined = Env.fromList [
    (Unqualified "+", Forall [] (TArr typeInt (TArr typeInt typeInt))),
    (Unqualified "-", Forall [] (TArr typeInt (TArr typeInt typeInt))),
    (Unqualified "*", Forall [] (TArr typeInt (TArr typeInt typeInt))),
    (Unqualified "/", Forall [] (TArr typeInt (TArr typeInt typeInt))),
    (Unqualified "<", Forall [] (TArr typeInt (TArr typeInt typeBool))),
    (Unqualified ">", Forall [] (TArr typeInt (TArr typeInt typeBool))),
    (Unqualified "<=", Forall [] (TArr typeInt (TArr typeInt typeBool))),
    (Unqualified ">=", Forall [] (TArr typeInt (TArr typeInt typeBool))),

    (Unqualified "and", Forall [] (TArr typeBool (TArr typeBool typeBool))),
    (Unqualified "or", Forall [] (TArr typeBool (TArr typeBool typeBool))),
    (Unqualified "not", Forall [] (TArr typeBool typeBool)),

    (Unqualified "==", Forall [TV "a"] (TArr (TVar (TV "a")) (TArr (TVar (TV "a")) typeBool))),

    (Unqualified "++", Forall [] (TArr typeString (TArr typeString typeString))),


    -- TODO: Remove when import actually does something
    (Qualified "fmt" "Println", Forall [TV "a"] (TArr (TVar (TV "a")) typeUnit))
  ]

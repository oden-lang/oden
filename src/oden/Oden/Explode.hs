module Oden.Explode
(
 ExplodeError(..),
 explodeExpr,
 explodeDefinitions,
 explodePackage
) where

import qualified Oden.Core.Untyped     as Untyped
import           Oden.Identifier
import           Oden.Syntax
import           Oden.Type.Polymorphic

import qualified Data.Map              as Map
import qualified Data.Set              as Set

data ExplodeError = TypeSignatureWithoutDefinition Name Scheme
                  deriving (Show, Eq)

explodeExpr :: Expr -> Untyped.Expr
explodeExpr (Symbol i) =
  Untyped.Symbol i
explodeExpr (Literal (Bool b)) =
  Untyped.Literal (Untyped.Bool b)
explodeExpr (Literal (Int i)) =
  Untyped.Literal (Untyped.Int i)
explodeExpr (Literal (String s)) =
  Untyped.Literal (Untyped.String s)
explodeExpr (If c t f) =
  Untyped.If (explodeExpr c) (explodeExpr t) (explodeExpr f)
-- (f x y z)
explodeExpr (Application f ps) =
  Untyped.Application (explodeExpr f) (map explodeExpr ps)
explodeExpr (Fn [] b) =
  Untyped.NoArgFn (explodeExpr b)
explodeExpr (Fn [arg] b) =
  Untyped.Fn arg (explodeExpr b)
explodeExpr (Fn (arg:args) b) =
  Untyped.Fn arg (explodeExpr (Fn args b))
-- invalid, but can be handled anyway
explodeExpr (Let [] b) = explodeExpr b
explodeExpr (Let [(n, e)] b) =
  Untyped.Let n (explodeExpr e) (explodeExpr b)
explodeExpr (Let ((n, e):bs) b) =
  Untyped.Let n (explodeExpr e) (explodeExpr (Let bs b))
explodeExpr (Slice es) =
  Untyped.Slice (map explodeExpr es)

explodeImport :: Import -> Untyped.Import
explodeImport (Import name) = Untyped.Import name

explodeType :: TypeExpr -> Type
explodeType TEAny = TAny
explodeType (TEVar s) = TVar (TV s)
explodeType (TECon s) = TCon s
explodeType (TEFn d r) = TFn (explodeType d) (explodeType r)
explodeType (TENoArgFn r) = TNoArgFn (explodeType r)
explodeType (TESlice t) = TSlice (explodeType t)

explodeScheme :: SchemeExpr -> Scheme
explodeScheme (Implicit t) =
  let t' = explodeType t
  in Forall (Set.toList (ftv t')) t'
explodeScheme (Explicit vars t) =
  let t' = explodeType t
  in Forall (map TV vars) t'

explodeDefinitions :: [Definition] -> Either [ExplodeError] [Untyped.Definition]
explodeDefinitions ds =
  let (scs, defs) = foldl iter (Map.empty, []) ds
  in case Map.assocs scs of
    [] -> Right defs
    as -> Left (map (uncurry TypeSignatureWithoutDefinition) as)
  where iter (ts, defs) (FnDefinition name args body) =
          let def = Untyped.Definition name (Map.lookup name ts) (explodeExpr (Fn args body))
          in (Map.delete name ts, defs ++ [def])
        iter (ts, defs) (ValueDefinition name expr) =
          let def = Untyped.Definition name (Map.lookup name ts) (explodeExpr expr)
          in (Map.delete name ts, defs ++ [def])
        iter (ts, defs) (TypeSignature name sc) =
          (Map.insert name (explodeScheme sc) ts, defs)

explodePackage :: Package -> Either [ExplodeError] Untyped.Package
explodePackage (Package name imports definitions) = do
  let is = map explodeImport imports
  ds <- explodeDefinitions definitions
  return (Untyped.Package name is ds)

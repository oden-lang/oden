module Oden.Explode
(
 ExplodeError(..),
 explodeExpr,
 explodeTopLevel,
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
explodeExpr (Op o e1 e2) =
  Untyped.Op o (explodeExpr e1) (explodeExpr e2)
explodeExpr (Symbol i) =
  Untyped.Symbol i
explodeExpr (Literal (Bool b)) =
  Untyped.Literal (Untyped.Bool b)
explodeExpr (Literal (Int i)) =
  Untyped.Literal (Untyped.Int i)
explodeExpr (Literal (String s)) =
  Untyped.Literal (Untyped.String s)
explodeExpr (Literal Unit) =
  Untyped.Literal Untyped.Unit
explodeExpr (If c t f) =
  Untyped.If (explodeExpr c) (explodeExpr t) (explodeExpr f)
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
explodeExpr (Block es) =
  Untyped.Block (map explodeExpr es)

explodeType :: TypeExpr -> Type
explodeType TEAny = TAny
explodeType TEUnit = TUnit
explodeType (TEVar s) = TVar (TV s)
explodeType (TECon s) = TCon s
explodeType (TEFn d []) = explodeType d
explodeType (TEFn d (r:rs)) =
  TFn (explodeType d) (explodeType (TEFn r rs))
explodeType (TENoArgFn r) = TNoArgFn (explodeType r)
explodeType (TESlice t) = TSlice (explodeType t)

explodeScheme :: SchemeExpr -> Scheme
explodeScheme (Implicit t) =
  let t' = explodeType t
  in Forall (Set.toList (ftv t')) t'
explodeScheme (Explicit vars t) =
  let t' = explodeType t
  in Forall (map TV vars) t'

explodeTopLevel :: [TopLevel] -> Either [ExplodeError] ([Untyped.Import], [Untyped.Definition])
explodeTopLevel top =
  let (is, scs, defs) = foldl iter ([], Map.empty, []) top
  in case Map.assocs scs of
    [] -> Right (is, defs)
    as -> Left (map (uncurry TypeSignatureWithoutDefinition) as)
  where iter (is, ts, defs) (FnDefinition name args body) =
          let def = Untyped.Definition name (Map.lookup name ts) (explodeExpr (Fn args body))
          in (is, Map.delete name ts, defs ++ [def])
        iter (is, ts, defs) (ValueDefinition name expr) =
          let def = Untyped.Definition name (Map.lookup name ts) (explodeExpr expr)
          in (is, Map.delete name ts, defs ++ [def])
        iter (is, ts, defs) (TypeSignature name sc) =
          (is, Map.insert name (explodeScheme sc) ts, defs)
        iter (is, ts, defs) (ImportDeclaration name) =
          (is ++ [Untyped.Import name], ts, defs)

explodePackage :: Package -> Either [ExplodeError] Untyped.Package
explodePackage (Package name definitions) = do
  (is, ds) <- explodeTopLevel definitions
  return (Untyped.Package name is ds)

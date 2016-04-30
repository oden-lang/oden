{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, LambdaCase #-}
module Oden.Pretty where

import           Oden.Core.Typed as Typed
import           Oden.Core.Definition
import           Oden.Core.Expr
import qualified Oden.Core.Untyped as Untyped
import           Oden.Core.Package
import           Oden.Core.Resolved
import           Oden.Core.Operator
import           Oden.Core.Monomorphed as Monomorphed

import           Oden.Identifier
import           Oden.QualifiedName    (QualifiedName(..))
import qualified Oden.Type.Monomorphic as Mono
import qualified Oden.Type.Polymorphic as Poly
import           Oden.Type.Signature

import           Data.List             (intersperse)
import           Data.Set              (toList)

import           Text.PrettyPrint.Leijen

commaSep :: Pretty p => [p] -> Doc
commaSep ps = hcat (punctuate (text ", ") (map pretty ps))

commaSepParens :: Pretty p => [p] -> Doc
commaSepParens = parens . commaSep

rArr :: Doc
rArr = text "->"

indentedInBraces :: Doc -> Doc
indentedInBraces d = vcat [lbrace, indent 4 d, rbrace]

instance Pretty NameBinding where
  pretty (NameBinding _ identifier) = pretty identifier

instance Pretty UnaryOperator where
  pretty Positive = text "+"
  pretty Negative = text "-"
  pretty Not      = text "!"

instance Pretty BinaryOperator where
  pretty Add = text "+"
  pretty Subtract = text "-"
  pretty Multiply = text "*"
  pretty Divide = text "/"
  pretty Equals = text "=="
  pretty Concat = text "++"
  pretty LessThan = text "<"
  pretty GreaterThan = text ">"
  pretty LessThanEqual = text "<="
  pretty GreaterThanEqual = text ">="
  pretty And = text "&&"
  pretty Or = text "||"

instance Pretty Identifier where
  pretty (Identifier n) = text n

instance Pretty e => Pretty (FieldInitializer e) where
  pretty (FieldInitializer _ l e) = pretty l <+> text "=" <+> pretty e

instance Pretty UnresolvedMethodReference where
  pretty (UnresolvedMethodReference (Poly.Protocol _ (FQN _ protocolName) _ _) (Poly.ProtocolMethod _ methodName _)) =
    pretty protocolName <> text "::" <> pretty methodName

instance Pretty ResolvedMethodReference where
  pretty (ResolvedMethodReference (Poly.Protocol _ (FQN _ protocolName) _ _) (Poly.ProtocolMethod _ methodName _) _) =
    pretty protocolName <> text "::" <> pretty methodName

instance (Pretty r, Pretty m) => Pretty (Expr r t m) where
  pretty (Symbol _ i _) = pretty i
  pretty (Subscript _ s i _) = pretty s <> text "[" <> pretty i <> text "]"
  pretty (Subslice _ s r _) = pretty s <> pretty r
  pretty (UnaryOp _ op e _) = pretty op <+> pretty e
  pretty (BinaryOp _ op e1 e2 _) = parens (pretty e1 <+> pretty op <+> pretty e2)
  pretty (Application _ f a _) = pretty f <> text "(" <> pretty a <> text ")"
  pretty (NoArgApplication _ f _) = pretty f <> text "()"
  pretty (ForeignFnApplication _ f as _) = pretty f <> commaSepParens as
  pretty (Fn _ n b _) = parens (parens (pretty n) <+> rArr <+> pretty b)
  pretty (NoArgFn _ b _) = parens empty <+> rArr <+> pretty b
  pretty (Let _ n e b _) =
    text "let" <+> pretty n <+> equals <+> pretty e <+> text "in" <+> pretty b
  pretty (Literal _ (Int n) _) = integer n
  pretty (Literal _ (Bool True) _) = text "true"
  pretty (Literal _ (Bool False) _) = text "false"
  pretty (Literal _ (String s) _) = text (show s)
  pretty (Literal _ Unit{} _) = text "()"
  pretty (Tuple _ f s r _) = commaSepParens (f:s:r)
  pretty (If _ c t e _) =
    text "if" <+> pretty c <+> text "then" <+> pretty t <+> text "else" <+> pretty e
  pretty (Slice _ es _) =
    text "[]" <> braces (hcat (punctuate (text ", ") (map pretty es)))
  pretty (Block _ es _) =
    braces (vcat (map pretty es))
  pretty (RecordInitializer _ fields _) =
    braces (hcat (punctuate (text ", ") (map pretty fields)))
  pretty (MemberAccess _ access _) = pretty access
  pretty (MethodReference _ ref _) = pretty ref

collectCurried :: Expr r t m -> ([NameBinding], Expr r t m)
collectCurried (Fn _ param body _) =
  let (params, body') = collectCurried body
  in (param:params, body')
collectCurried expr = ([], expr)

prettyDefinition :: (Pretty r, Pretty t, Pretty m) => Identifier -> Expr r t m -> Doc
prettyDefinition name (NoArgFn _ body _) =
  pretty name <> parens empty <+> equals <+> pretty body
prettyDefinition name expr =
  case collectCurried expr of
    ([], body) ->
      pretty name <+> equals <+> pretty body
    (params, body) ->
      pretty name <> commaSepParens params <+> equals <+> pretty body

instance Pretty e => Pretty (Range e) where
  pretty = \case
    Range e1 e2 -> brackets $ pretty e1 <+> text ":" <+> pretty e2
    RangeTo e   -> brackets $ text ":" <+> pretty e
    RangeFrom e -> brackets $ pretty e <+> text ":"

instance Pretty CanonicalExpr where
  pretty (scheme, expr) = pretty expr <+> text ":" <+> pretty scheme

instance Pretty Poly.ProtocolMethod where
  pretty (Poly.ProtocolMethod _ name scheme) =
    pretty name <+> colon <+> pretty scheme

instance Pretty Poly.Protocol where
  pretty (Poly.Protocol _ (FQN _ name) tvar methods) =
    text "protocol" <+> pretty name <> parens (pretty tvar)
      <+> indentedInBraces (vcat (map pretty methods))

instance (Pretty r, Pretty t, Pretty m) => Pretty (Definition (Expr r t m)) where
  pretty (Definition _ name (scheme, expr)) = vcat [
      pretty name <+> text ":" <+> pretty scheme,
      prettyDefinition name expr
    ]
  pretty (ForeignDefinition _ name scheme) = vcat [
      text "// (foreign)",
      text "//" <+> pretty name <+> text ":" <+> pretty scheme
    ]
  pretty (TypeDefinition _ name _ type') =
    text "type" <+> pretty name <+> equals <+> pretty type'
  pretty (ProtocolDefinition _ _ protocol) = pretty protocol

instance Pretty PackageName where
  pretty parts = hcat (punctuate (text "/") (map text parts))

instance Pretty PackageDeclaration where
  pretty (PackageDeclaration _ name) = text "package" <+> pretty name

instance Pretty (ImportedPackage ResolvedPackage) where
  pretty (ImportedPackage _ _ (ResolvedPackage (PackageDeclaration _ pkgName) _ _)) =
    text "import" <+> pretty pkgName

instance Pretty Typed.TypedPackage  where
  pretty (Typed.TypedPackage decl imports defs) =
    vcat (punctuate line (pretty decl : map pretty imports ++ map pretty defs))

instance Pretty Poly.TVar where
  pretty (Poly.TV s) = text s

instance Pretty Poly.TVarBinding where
  pretty (Poly.TVarBinding _ v) = pretty v

instance Pretty QualifiedName where
  pretty (FQN pkg identifier) = hcat (punctuate (text ".") (map text pkg ++ [pretty identifier]))

isFunctionType :: Poly.Type -> Bool
isFunctionType = \case
  Poly.TFn{}        -> True
  Poly.TNoArgFn{}   -> True
  Poly.TForeignFn{} -> True
  _                 -> False

parensIfFunction :: Poly.Type -> Doc
parensIfFunction t
  | isFunctionType t = parens (pretty t)
  | otherwise        = pretty t

instance Pretty Poly.Type where
  pretty (Poly.TTuple _ f s r) = commaSepParens (f:s:r)
  pretty (Poly.TVar _ v) = pretty v
  pretty (Poly.TCon _ (FQN [] (Identifier "unit"))) = text "()"
  pretty (Poly.TCon _ n) = pretty n
  pretty (Poly.TNoArgFn _ t) = rArr <+> parensIfFunction t
  pretty (Poly.TFn _ tf ta) = parensIfFunction tf <+> rArr <+> pretty ta
  pretty (Poly.TForeignFn _ _ ps rs) =
    text "<foreign>"
    <+> hsep (intersperse (text "&") (map parensIfFunction ps))
    <+> rArr <+> ppReturns rs
  pretty (Poly.TSlice _ t) =
    text "[]" <> braces (pretty t)
  pretty (Poly.TNamed _ n _) = pretty n
  pretty (Poly.TConstrained constraints t) =
    parens (pretty t <+> text "where" <+> commaSep (toList constraints))
  pretty (Poly.TRecord _ r) = braces (ppFields r)
  pretty Poly.REmpty{} = braces empty
  pretty r@Poly.RExtension{} = parens (ppFields r)

ppFields :: Poly.Type -> Doc
ppFields r =
  case Poly.getLeafRow r of
    var@Poly.TVar{} -> printFields r <+> text "|" <+> pretty var
    _ -> printFields r
  where
  printFields = hcat . punctuate (text ", ") . map printField . Poly.rowToList
  printField (label, type') = pretty label <> colon <+> pretty type'

instance Pretty Poly.ProtocolConstraint where
  pretty (Poly.ProtocolConstraint _ (Poly.Protocol _ (FQN _ name) _ _) type') =
    pretty name <> parens (pretty type')

instance Pretty Poly.Scheme where
  pretty (Poly.Forall _ quantifiers constraints t) =
    let qs = if null quantifiers
             then empty
             else text "forall" <+> hsep (map pretty quantifiers) <> text ". "
        cs = if null constraints
             then empty
             else text " where" <+> commaSep (toList constraints)
    in qs <> pretty t <> cs

ppMonoFields :: Mono.Type -> Doc
ppMonoFields Mono.REmpty{} = empty
ppMonoFields r = getPairs r
  where
  getPairs (Mono.RExtension _ label type' Mono.REmpty{}) =
    pretty label <> colon <+> pretty type'
  getPairs (Mono.RExtension _ label type' row) =
    pretty label <> colon <+> pretty type' <> comma <+> getPairs row
  getPairs _ = empty

ppReturns :: Pretty p => [p] -> Doc
ppReturns [r] = pretty r
ppReturns rs = commaSepParens rs

instance Pretty Mono.Type where
  pretty (Mono.TTuple _ f s r) =
    brackets (hcat (punctuate (text ", ") (map pretty (f:s:r))))
  pretty (Mono.TCon _ (FQN [] (Identifier "unit"))) = text "()"
  pretty (Mono.TCon _ n) = pretty n
  pretty (Mono.TNoArgFn _ t) = rArr <+> pretty t
  pretty (Mono.TFn _ tf ta) = pretty tf <+> rArr <+> pretty ta
  pretty (Mono.TForeignFn _ _ ps rs) = text "<foreign>" <+> hsep (intersperse (text "&") (map pretty ps)) <+> rArr <+> ppReturns rs
  pretty (Mono.TSlice _ t) =
    text "[]" <> braces (pretty t)
  pretty (Mono.TNamed _ n _) = pretty n
  pretty (Mono.TRecord _ r) = braces (ppMonoFields r)
  pretty Mono.REmpty{} = braces empty
  pretty r@Mono.RExtension{} = parens (ppMonoFields r)

instance Pretty (SignatureVarBinding a) where
  pretty (SignatureVarBinding _ s) = pretty s

instance Pretty (SignatureExpr a) where
  pretty (TSUnit _) = text "()"
  pretty (TSSymbol _ i) = pretty i
  pretty (TSApp _ d r) = pretty d <> parens (pretty r)
  pretty (TSNoArgFn _ t) = rArr <+> pretty t
  pretty (TSFn _ tf ta) = pretty tf <+> rArr <+> pretty ta
  pretty (TSTuple _ f s r) =
    brackets (hcat (punctuate (text ", ") (map pretty (f:s:r))))
  pretty (TSSlice _ t) =
    text "!" <> braces (pretty t)
  pretty (TSRowEmpty _) = empty
  pretty (TSRowExtension _ label type' TSRowEmpty{}) = pretty label <> colon <+> pretty type'
  pretty (TSRowExtension _ label type' row) = pretty label <> colon <+> pretty type' <> comma <+> pretty row
  -- TODO: Better printing with braces
  pretty (TSRecord _ row) = text "record " <> pretty row

instance Pretty (TypeSignature a) where
  pretty (TypeSignature _ [] expr) = pretty expr
  pretty (TypeSignature _ vars expr) =
    text "forall" <+> hsep (map pretty vars) <> text "." <+> pretty expr

instance Pretty TypedMemberAccess where
  pretty (Typed.RecordFieldAccess expr name) =
    pretty expr <> text "." <> pretty name
  pretty (Typed.PackageMemberAccess expr name) =
    pretty expr <> text "." <> pretty name

instance Pretty MonoTypedMemberAccess where
  pretty (Monomorphed.RecordFieldAccess expr name) =
    pretty expr <> text "." <> pretty name
  pretty (Monomorphed.PackageMemberAccess expr name) =
    pretty expr <> text "." <> pretty name

instance Pretty InstantiatedDefinition where
  pretty (InstantiatedDefinition polyName _si name expr) = vcat [
      text "//" <+> pretty polyName,
      pretty name <+> text ":" <+> pretty (typeOf expr),
      prettyDefinition name expr
    ]

instance Pretty MonomorphedDefinition where
  pretty (MonomorphedDefinition _ name _ expr) = vcat [
      pretty name <+> text ":" <+> pretty (typeOf expr),
      prettyDefinition name expr
    ]

instance Pretty MonomorphedPackage where
  pretty (MonomorphedPackage decl imports is ms) =
    vcat (punctuate line parts)
    where parts = pretty decl : map pretty imports
                              ++ map pretty (toList is)
                              ++ map pretty (toList ms)

instance Pretty Untyped.NamedMethodReference where
  pretty (Untyped.NamedMethodReference protocolName methodName) =
    pretty protocolName <> text "::" <> pretty methodName

instance Pretty Untyped.NamedMemberAccess where
  pretty (Untyped.NamedMemberAccess expr member) =
    pretty expr <> text "." <> pretty member

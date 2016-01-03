module Oden.Backend.Go where

import qualified Data.Set              as Set
import           System.FilePath
import           Text.PrettyPrint

import           Oden.Backend
import           Oden.Compiler
import           Oden.Core
import           Oden.Identifier
import qualified Oden.Type.Monomorphic as Mono

newtype GoBackend = GoBackend FilePath

block :: Doc -> Doc
block d = lbrace $+$ nest 4 d $+$ rbrace

funcArg :: Name -> Mono.Type -> Doc
funcArg name t = safeName name <+> codegenType t

func :: Doc -> Doc -> Doc -> Doc -> Doc
func name arg returnType body =
  text "func"
  <+> name
  <> parens arg
  <+> returnType
  <+> if isEmpty body then empty else block body

var :: Name -> Expr Mono.Type -> Doc
var name expr =
  text "var"
  <+> safeName name
  <+> codegenType (typeOf expr)
  <+> equals
  <+> codegenExpr expr

return' :: Expr Mono.Type -> Doc
return' e@(NoArgApplication _ t) | t == Mono.typeUnit =
  codegenExpr e $+$ text "return"
return' e@(Application _ _ t) | t == Mono.typeUnit =
  codegenExpr e $+$ text "return"
return' e@(Let _ _ _ t) | t == Mono.typeUnit =
  codegenExpr e $+$ text "return"
return' (Symbol (Unqualified "unit") _) =
  text "return"
return' e@(Symbol _ t) | t == Mono.typeUnit =
  codegenExpr e $+$ text "return"
return' expr = text "return" <+> codegenExpr expr

replaceIdentifierPart :: Char -> String
replaceIdentifierPart '-' = "_DASH_"
replaceIdentifierPart '\'' = "_PRIM_"
replaceIdentifierPart '!' = "_BANG_"
replaceIdentifierPart '$' = "_DLLR_"
replaceIdentifierPart '&' = "_AMPR_"
replaceIdentifierPart '*' = "_STAR_"
replaceIdentifierPart '+' = "_PLUS_"
replaceIdentifierPart '<' = "_LT_"
replaceIdentifierPart '=' = "_EQ_"
replaceIdentifierPart '>' = "_GT_"
replaceIdentifierPart '?' = "_QST_"
replaceIdentifierPart '^' = "_CRCM_"
replaceIdentifierPart '|' = "_PIPE_"
replaceIdentifierPart '~' = "_TLDE_"
replaceIdentifierPart c = [c]

safeName :: Name -> Doc
safeName = text . concatMap replaceIdentifierPart

codegenIdentifier :: Identifier -> Doc
codegenIdentifier (Unqualified n) = safeName n
codegenIdentifier (Qualified pn n) = safeName pn <> text "." <> safeName n

codegenType :: Mono.Type -> Doc
codegenType t | t == Mono.typeUnit = empty
codegenType (Mono.TCon n) = safeName n
codegenType (Mono.TArrSingle f) =
  func empty empty (codegenType f) empty
codegenType (Mono.TArr d r) =
  func empty (codegenType d) (codegenType r) empty
codegenType (Mono.TSlice t) =
  text "[]" <> codegenType t
codegenType (Mono.TGoFunc as r) =
  func empty (hcat (punctuate (text ", ") (map codegenType as))) (codegenType r) empty

isInfix :: Expr Mono.Type -> Bool
isInfix (Symbol (Unqualified s) _) = s `elem` ["+", "-", "*", "/", "==", ">", "<", "<=", ">=", "++", "and", "or"]
isInfix _ = False

codegenOperator :: Expr Mono.Type -> Doc
codegenOperator (Symbol (Unqualified "++") _) = text "+"
codegenOperator (Symbol (Unqualified "and") _) = text "&&"
codegenOperator (Symbol (Unqualified "or") _) = text "||"
codegenOperator (Symbol (Unqualified s) _) | s `elem` ["+", "-", "*", "/", "==", ">", "<", "<=", ">=", "++"] = text s
codegenOperator e = codegenExpr e

codegenExpr :: Expr Mono.Type -> Doc
codegenExpr (Symbol i _) =
  codegenIdentifier i
codegenExpr (Application (Symbol (Unqualified "not") _) e _) =
  text "!" <> codegenExpr e
codegenExpr (Application (Application o p1 _) p2 _) | isInfix o =
  parens (codegenOperator p1 <+> codegenOperator o <+> codegenOperator p2)
codegenExpr (Application f p _) =
  codegenExpr f <> parens (codegenExpr p)
codegenExpr (GoFuncApplication f p _) =
  codegenExpr f <> parens (codegenExpr p)
codegenExpr (NoArgApplication f _) =
  codegenExpr f <> parens empty
codegenExpr (Fn a body (Mono.TArr d r)) =
  func empty (funcArg a d) (codegenType r) (return' body)
codegenExpr (NoArgFn body (Mono.TArrSingle r)) =
  func empty empty (codegenType r) (return' body)
codegenExpr (Let n expr body t) =
  parens (func empty empty (codegenType t) (text "var" <+> safeName n <+> codegenType (typeOf expr)<+> equals <+> codegenExpr expr $+$ return' body))
  <> parens empty
codegenExpr (Literal (Int n) _) = integer n
codegenExpr (Literal (Bool True) _) = text "true"
codegenExpr (Literal (Bool False) _) = text "false"
codegenExpr (Literal (String s) _) = text (show s)
codegenExpr (If condExpr thenExpr elseExpr t) =
  parens
  (func empty empty (codegenType t) (text "if" <+> codegenExpr condExpr <+> block (return' thenExpr) <+> text "else" <+> block (return' elseExpr)))
  <> parens empty
codegenExpr (Slice exprs t) = codegenType t
                            <> braces (hcat (punctuate (comma <+> space) (map codegenExpr exprs)))

codegenTopLevel :: Name -> Expr Mono.Type -> Doc
codegenTopLevel name (NoArgFn body (Mono.TArrSingle r)) =
  func (safeName name) empty (codegenType r) (return' body)
codegenTopLevel name (Fn a body (Mono.TArr d r)) =
  func (safeName name) (funcArg a d) (codegenType r) (return' body)
codegenTopLevel name expr =
  var name expr

codegenInstance :: InstantiatedDefinition -> Doc
codegenInstance (InstantiatedDefinition name expr) =
  codegenTopLevel name expr

codegenMonomorphed :: MonomorphedDefinition -> Doc
codegenMonomorphed (MonomorphedDefinition name expr) =
  codegenTopLevel name expr

codegenImport :: Import -> Doc
codegenImport (Import name) =
  text "import" <+> doubleQuotes (hcat (punctuate (text "/") (map text name)))

codegenPackage :: CompiledPackage -> Doc
codegenPackage (CompiledPackage name imports instances monomorphed) =
  text "package" <+> text (last name)
  $+$ vcat (map codegenImport imports)
  $+$ vcat (map codegenInstance (Set.toList instances))
  $+$ vcat (map codegenMonomorphed (Set.toList monomorphed))

toFilePath :: GoBackend -> PackageName -> Either CodegenError FilePath
toFilePath _ [] = Left (UnexpectedError "Package name cannot be empty")
toFilePath (GoBackend goPath) parts =
  let isMain = last parts == "main"
      dirParts = "src" : if isMain then init parts else parts
      dir = foldl (</>) goPath dirParts
      fileName = if isMain then "main.go" else last parts ++ ".go"
  in return (dir </> fileName)

instance Backend GoBackend where
  codegen backend pkg@(CompiledPackage name _ _ _) = do
    path <- toFilePath backend name
    return [CompiledFile path (render (codegenPackage pkg))]

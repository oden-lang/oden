{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Oden.Backend.Go (
  GoBackend(..),
  prelude,
  genPackage
) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.List             (sortOn, find)
import           Data.Maybe            (maybeToList)
import qualified Data.Set              as Set

import           Text.PrettyPrint.Leijen (renderPretty, displayS, pretty)
import           System.FilePath

import qualified Oden.Go.AST           as AST
import qualified Oden.Go.Identifier    as GI
import qualified Oden.Go.Type          as GT
import           Oden.Go.Pretty ()

import           Oden.Backend

import           Oden.Core.Expr
import           Oden.Core.Monomorphed as Monomorphed
import           Oden.Core.Package
import           Oden.Core.Foreign

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName    (PackageName(..), QualifiedName(..))
import           Oden.SourceInfo       hiding (fileName)
import qualified Oden.SourceInfo       as SourceInfo
import qualified Oden.Type.Monomorphic as Mono

type Codegen = ReaderT MonomorphedPackage (Except CodegenError)

newtype GoBackend = GoBackend FilePath

isUniverseTypeConstructor :: String -> Mono.Type -> Bool
isUniverseTypeConstructor expected (Mono.TCon _ (FQN (NativePackageName []) (Identifier actual))) =
  actual == expected
isUniverseTypeConstructor _ _ = False

replaceIdentifierPart :: Char -> String
replaceIdentifierPart c = case c of
  '-'  -> "_DASH_"
  '\'' -> "_PRIM_"
  '!'  -> "_BANG_"
  '$'  -> "_DLLR_"
  '&'  -> "_AMPR_"
  '*'  -> "_STAR_"
  '+'  -> "_PLUS_"
  '<'  -> "_LT_"
  '='  -> "_EQ_"
  '>'  -> "_GT_"
  '?'  -> "_QST_"
  '^'  -> "_CRCM_"
  '|'  -> "_PIPE_"
  '~'  -> "_TLDE_"
  _    -> [c]

safeName :: String -> GI.Identifier
safeName = GI.Identifier . concatMap replaceIdentifierPart

genSourceInfo :: SourceInfo -> AST.Comment
genSourceInfo =
  \case
    Missing ->
      AST.CompilerDirective "line <missing>:0"
    Predefined ->
      AST.CompilerDirective "line <predefined>:0"
    SourceInfo pos ->
      AST.CompilerDirective
      ("line "
       ++ SourceInfo.fileName pos
       ++ ":" ++ show (line pos))

genSourceInfo' :: Metadata SourceInfo -> AST.Comment
genSourceInfo' = genSourceInfo . unwrap

genIdentifier :: Identifier -> Codegen GI.Identifier
genIdentifier (Identifier s) = return $ safeName s

genType :: Mono.Type -> Codegen GT.Type
genType t@Mono.TCon{}
  | isUniverseTypeConstructor "unit" t = return $ GT.Struct []
  | isUniverseTypeConstructor "int" t = return $ GT.Basic (GI.Identifier "int") False
  | isUniverseTypeConstructor "float64" t = return $ GT.Basic (GI.Identifier "float64") False
  | isUniverseTypeConstructor "bool" t = return $ GT.Basic (GI.Identifier "bool") False
  | isUniverseTypeConstructor "string" t = return $ GT.Basic (GI.Identifier "string") False
  | otherwise = throwError (UnexpectedError $ "Unsupported type constructor: " ++ show t)
genType t@Mono.TApp{} =
  throwError $ UnexpectedError $ "Type application not supported yet: " ++ show t
genType (Mono.TTuple _ f s r) =
  GT.Struct <$> zipWithM genTupleField [0..] (f:s:r)
  where
  genTupleField :: Int -> Mono.Type -> Codegen GT.StructField
  genTupleField n t = GT.StructField (GI.Identifier ("_" ++ show n)) <$> genType t
genType (Mono.TNoArgFn _ f) = do
  fc <- genType f
  return (GT.Signature Nothing (GT.Parameters [] False) (GT.Returns [fc]))
genType (Mono.TFn _ d r) = do
  dc <- genType d
  rc <- genType r
  return (GT.Signature Nothing (GT.Parameters [dc] False) (GT.Returns [rc]))
genType (Mono.TSlice _ t) =
  GT.Slice <$> genType t
genType (Mono.TForeignFn _ isVariadic ps rs) = do
  ps' <- mapM genType ps
  rs' <- mapM genType rs
  return (GT.Signature Nothing (GT.Parameters ps' isVariadic) (GT.Returns rs'))
genType (Mono.TRecord _ row) = genType row
genType (Mono.TNamed _ _ t) = genType t
genType Mono.REmpty{} = return $ GT.Struct []
genType row@Mono.RExtension{} = do
  let fields = sortOn fst (Mono.rowToList row)
  GT.Struct <$> mapM genField fields
  where genField (Identifier name, t) = GT.StructField (safeName name) <$> genType t

genBinaryOperator :: BinaryOperator -> AST.BinaryOperator
genBinaryOperator op = case op of
  Add -> AST.Sum
  Subtract -> AST.Difference
  Multiply -> AST.Product
  Divide -> AST.Quotient
  Equals -> AST.EQ
  NotEquals -> AST.NEQ
  Concat -> AST.Sum
  LessThan -> AST.LT
  GreaterThan -> AST.GT
  LessThanEqual -> AST.LTE
  GreaterThanEqual -> AST.GTE
  And -> AST.And
  Or -> AST.Or

genUnaryOperator :: UnaryOperator -> AST.UnaryOperator
genUnaryOperator Negate = AST.UnaryNegation
genUnaryOperator Not = AST.Not

genRange :: MonoTypedRange -> Codegen AST.SliceExpression
genRange (Range e1 e2) = AST.ClosedSlice <$> genExpr e1 <*> genExpr e2
genRange (RangeFrom e) = AST.LowerBoundSlice <$> genExpr e
genRange (RangeTo e) = AST.UpperBoundSlice <$> genExpr e

-- | Generates a call to a foreign function.
genRawForeignFnApplication :: MonoTypedExpr -> [MonoTypedExpr] -> Codegen AST.PrimaryExpression
genRawForeignFnApplication f args =
  case typeOf f of
    Mono.TForeignFn _ True _ _ -> do
      let nonVariadicArgs = init args
          slice = last args
      fc <- genPrimaryExpression f
      args' <- map AST.Argument <$> mapM genExpr nonVariadicArgs
      variadicArg <- AST.VariadicSliceArgument <$> genExpr slice
      return (AST.Application fc (args' ++ [variadicArg]))
    _ -> AST.Application <$> genPrimaryExpression f
                         <*> (map AST.Argument <$> mapM genExpr args)

grouped :: AST.Expression -> AST.Expression
grouped = AST.Expression . AST.Operand . AST.GroupedExpression

operandName :: GI.Identifier -> AST.Expression
operandName = AST.Expression . AST.Operand . AST.OperandName

literalExpr :: AST.Literal -> AST.Expression
literalExpr = AST.Expression . AST.Operand . AST.Literal

-- | Generates an anonymous function that will take parameters of the specified
-- types and return a tuple containing those values.
genTupleWrapper :: (Mono.Type, Mono.Type, [Mono.Type]) -> Codegen AST.PrimaryExpression
genTupleWrapper (fstType, sndType, restTypes) = do
  let types = fstType : sndType : restTypes
      names = take (length types) (map (GI.Identifier . ("_" ++) . show) [(0 :: Int)..])

  paramTypes <- mapM genType types

  -- TODO: Create this type out of 'names' and AST constructors directly, don't
  -- genType to get it.
  tupleType <- genType (Mono.TTuple (Metadata Missing) fstType sndType restTypes)

  let params = zipWith AST.FunctionParameter names paramTypes
      tupleLiteral = AST.Expression
                     (AST.Operand
                      (AST.Literal
                       (AST.CompositeLiteral
                        tupleType
                        (AST.LiteralValueElements
                         (map (AST.UnkeyedElement . operandName) names)))))
  return (AST.Operand
          (AST.Literal
           (AST.FunctionLiteral
            (AST.FunctionSignature params [tupleType])
            (AST.Block [AST.ReturnStmt [tupleLiteral]]))))

-- | Generates a call to a foreign function with multiple return values,
-- returning a tuple.
genTupleWrappedForeignFnApplication :: MonoTypedExpr                      -- the function expr
                                    -> [MonoTypedExpr]                    -- function application arguments
                                    -> (Mono.Type, Mono.Type, [Mono.Type]) -- tuple types
                                    -> Codegen AST.Expression
genTupleWrappedForeignFnApplication f args returnTypes = do
  fnCall <- genRawForeignFnApplication f args
  wrapperFn <- genTupleWrapper returnTypes
  return (AST.Expression
          (AST.Application
           wrapperFn
           [AST.Argument (AST.Expression fnCall)]))

emptyStructLiteral :: AST.Expression
emptyStructLiteral =
  literalExpr
  (AST.CompositeLiteral
    (GT.Struct [])
    (AST.LiteralValueElements []))

-- | Wraps the provided code in a function that returns unit
voidToUnitWrapper :: SourceInfo -> AST.Expression -> AST.Expression
voidToUnitWrapper si expr =
  AST.Expression
  (AST.Application
   (AST.Operand
    (AST.Literal
     (AST.FunctionLiteral
      (AST.FunctionSignature [] [GT.Struct []])
      (AST.Block [ AST.StmtComment (genSourceInfo si)
                 , AST.SimpleStmt (AST.ExpressionStmt expr)
                 , AST.StmtComment (genSourceInfo si)
                 , AST.ReturnStmt [emptyStructLiteral]]))))
   [])

asPrimaryExpression :: AST.Expression -> AST.PrimaryExpression
asPrimaryExpression (AST.Expression primaryExpr) = primaryExpr
asPrimaryExpression nonPrimary = AST.Operand (AST.GroupedExpression nonPrimary)

genPrimaryExpression :: MonoTypedExpr -> Codegen AST.PrimaryExpression
genPrimaryExpression expr = asPrimaryExpression <$> genExpr expr

returnSingle :: AST.Expression -> AST.Stmt
returnSingle expr = AST.ReturnStmt [expr]

genExpr :: MonoTypedExpr -> Codegen AST.Expression
genExpr expr = case expr of
  Symbol _ i _ -> (AST.Expression . AST.Operand . AST.OperandName) <$> genIdentifier i
  Subscript _ s i _ ->
    AST.Expression <$> (AST.Index <$> genPrimaryExpression s <*> genExpr i)
  Subslice _ s r _ ->
    AST.Expression <$> (AST.Slice <$> genPrimaryExpression s <*> genRange r)

  -- Foreign operators are code generated in a special manner.
  Application _ (Foreign _ (ForeignUnaryOperator o) _) operand _ ->
    grouped <$> (AST.UnaryOp (genUnaryOperator o) <$> genPrimaryExpression operand)
  Application _ (Application _ (Foreign _ (ForeignBinaryOperator o) _) lhs _) rhs _ ->
    grouped <$> (AST.BinaryOp (genBinaryOperator o) <$> genExpr lhs <*> genExpr rhs)

  Application _ f arg _ ->
    AST.Expression <$> (AST.Application <$> genPrimaryExpression f
                                        <*> (((:[]) . AST.Argument) <$> genExpr arg))
  ForeignFnApplication (Metadata si) f args _ ->
    case typeOf f of

      -- Go functions that return unit are (almost always) void functions
      -- we wrap them to make the call to them return unit
      Mono.TForeignFn _ _ _ [t] | isUniverseTypeConstructor "unit" t -> do
        fnCall <- genRawForeignFnApplication f args
        return $ voidToUnitWrapper si (AST.Expression fnCall)

      Mono.TForeignFn _ _ _ (t1:t2:tr) ->
        genTupleWrappedForeignFnApplication f args (t1, t2, tr)

      -- Otherwise, just generate the call
      _ -> AST.Expression <$> genRawForeignFnApplication f args

  NoArgApplication _ f _->
    AST.Expression <$> (AST.Application <$> genPrimaryExpression f <*> return [])

  Fn _ (NameBinding _ paramName) body (Mono.TFn _ d r) -> do
    param <- AST.FunctionParameter <$> genIdentifier paramName <*> genType d
    returnType <- genType r
    returnStmt <- returnSingle <$> genExpr body
    return (literalExpr
            (AST.FunctionLiteral
             (AST.FunctionSignature [param] [returnType])
             (AST.Block [ AST.StmtComment (genSourceInfo (getSourceInfo body))
                        , returnStmt])))
  Fn _ _ _ t ->
    throwError $ UnexpectedError $ "Invalid fn type: " ++ show t

  NoArgFn _ body (Mono.TNoArgFn _ r) -> do
    returnType <- genType r
    returnStmt <- returnSingle <$> genExpr body
    return (literalExpr (AST.FunctionLiteral (AST.FunctionSignature [] [returnType]) (AST.Block [returnStmt])))

  NoArgFn _ _ t ->
    throwError $ UnexpectedError $ "Invalid no-arg fn type: " ++ show t

  Let _ (NameBinding (Metadata si) name) bindingExpr body letType -> do
    varDecl <- (AST.DeclarationStmt Nothing . AST.VarDecl)
               <$> (AST.VarDeclInitializer
                   <$> genIdentifier name
                   <*> genType (typeOf bindingExpr)
                   <*> genExpr bindingExpr)
    returnStmt <- returnSingle <$> genExpr body
    letType' <- genType letType
    let func = AST.Operand (AST.Literal
                            (AST.FunctionLiteral
                             (AST.FunctionSignature [] [letType'])
                             (AST.Block [ AST.StmtComment (genSourceInfo si)
                                        , varDecl
                                        , AST.StmtComment (genSourceInfo (getSourceInfo body))
                                        , returnStmt])))
    return (AST.Expression (AST.Application func []))

  Literal _ lit _ -> case lit of
    Int n      -> return $ literalExpr $ AST.BasicLiteral $ AST.IntLiteral n
    Float f    -> return $ literalExpr $ AST.BasicLiteral $ AST.FloatLiteral f
    Bool True  -> return $ operandName (GI.Identifier "true")
    Bool False -> return $ operandName (GI.Identifier "false")
    String s   -> return $ literalExpr $ AST.BasicLiteral $ AST.StringLiteral $ AST.InterpretedStringLiteral s
    Unit{}     -> return emptyStructLiteral

  Tuple _ f s r t -> do
    elements <- AST.LiteralValueElements . concat <$> mapM genTupleField (f:s:r)
    literalExpr <$> (AST.CompositeLiteral <$> genType t <*> return elements)
    where
    genTupleField e = do
      field <- AST.UnkeyedElement <$> genExpr e
      return [ AST.LiteralComment (genSourceInfo (getSourceInfo e))
             , field ]

  If _ condExpr thenExpr elseExpr exprType -> do
    condExpr' <- genExpr condExpr
    thenExpr' <- genExpr thenExpr
    let thenBranch = AST.Block
                     [ AST.StmtComment (genSourceInfo (getSourceInfo thenExpr))
                     , returnSingle thenExpr']
    elseExpr' <- genExpr elseExpr
    let elseBranch = AST.ElseBlock
                     (AST.Block
                      [ AST.StmtComment (genSourceInfo (getSourceInfo elseExpr))
                      , returnSingle elseExpr'])

    exprType' <- genType exprType

    let ifStmt = AST.IfStmt (AST.IfElse condExpr' thenBranch elseBranch)
    let func = AST.Operand (AST.Literal
                            (AST.FunctionLiteral
                             (AST.FunctionSignature [] [exprType'])
                             (AST.Block [ifStmt])))
    return (AST.Expression (AST.Application func []))

  Slice _ exprs t -> do
    sliceType <- genType t
    elements <- AST.LiteralValueElements . concat <$> mapM genSliceValue exprs
    return (literalExpr (AST.CompositeLiteral sliceType elements))
    where
    genSliceValue e = do
      value <- AST.UnkeyedElement <$> genExpr e
      return [ AST.LiteralComment (genSourceInfo (getSourceInfo e))
             , value ]

  Block _ [] _ ->
    return emptyStructLiteral

  Block _ exprs t -> do
    blockType <- genType t
    initStmts <- concat <$> mapM genWithSourceInfo (init exprs)
    let lastExpr = last exprs
    returnStmt <- returnSingle <$> genExpr lastExpr
    let stmts = initStmts ++ [ AST.StmtComment (genSourceInfo (getSourceInfo lastExpr))
                             , returnStmt
                             ]
    let func = AST.Operand
               (AST.Literal
                (AST.FunctionLiteral
                 (AST.FunctionSignature [] [blockType])
                 (AST.Block stmts)))
    return (AST.Expression (AST.Application func []))
    where
    genWithSourceInfo e = do
      exprStmt <- (AST.SimpleStmt . AST.ExpressionStmt) <$> genExpr e
      return [ AST.StmtComment (genSourceInfo (getSourceInfo e))
             , exprStmt
             ]

  RecordInitializer _ values recordType -> do
    elements <- (AST.LiteralValueElements . concat) <$> mapM genField values
    structType <- genType recordType
    return (literalExpr (AST.CompositeLiteral structType elements))
    where
    genField (FieldInitializer (Metadata si) label initializerExpr) = do
      keyed <- AST.KeyedElement <$> (AST.LiteralKeyName <$> genIdentifier label)
                                <*> genExpr initializerExpr
      return [ AST.LiteralComment (genSourceInfo si)
             , keyed ]

  MemberAccess _ access _ ->
    case access of
      Monomorphed.RecordFieldAccess recordExpr name -> do
        primaryExpr <- genPrimaryExpression recordExpr
        AST.Expression . AST.Selector primaryExpr <$> genIdentifier name

      Monomorphed.PackageMemberAccess pkgAlias name ->
        (AST.Expression . AST.Operand) <$> (AST.QualifiedOperandName <$> genIdentifier pkgAlias
                                                                 <*> genIdentifier name)

  -- TODO: is this needed? Monomorphed method references can be regular symbols
  -- at this phase?
  MethodReference _ _ref _ -> error "invalid protocol method reference in codegen phase"

  Foreign _ (ForeignSymbol s) _ ->
    (AST.Expression . AST.Operand . AST.OperandName) <$> genIdentifier s

  Foreign _ (ForeignUnaryOperator _) _ ->
    error "cannot codegen foreign unary operator without an application"

  Foreign _ (ForeignBinaryOperator _) _ ->
    error "cannot codegen foreign binary operator without a full binary application"


genBlock :: MonoTypedExpr -> Codegen AST.Block
genBlock expr = do
  returnStmt <- AST.ReturnStmt . (:[]) <$> genExpr expr
  let comment = AST.StmtComment (genSourceInfo (getSourceInfo expr))
  return (AST.Block [comment, returnStmt])


genTopLevel :: Identifier
            -> Mono.Type
            -> MonoTypedExpr
            -> Codegen [AST.TopLevelDeclaration]
genTopLevel (Identifier "main") (Mono.TNoArgFn _ t) (NoArgFn si body _) | isUniverseTypeConstructor "unit" t = do
  block <- case body of
    Block _ [] _ -> return (AST.Block [])
    _          -> do
      bodyStmt <- AST.SimpleStmt . AST.ExpressionStmt <$> genExpr body
      return (AST.Block [ AST.StmtComment (genSourceInfo (getSourceInfo body))
                        , bodyStmt ])
  return [ AST.TopLevelComment (genSourceInfo' si)
         , AST.FunctionDecl (GI.Identifier "main") (AST.FunctionSignature [] []) block]
genTopLevel name _ (NoArgFn si body (Mono.TNoArgFn _ returnType)) = do
  name' <- genIdentifier name
  returnType' <- genType returnType
  function <- AST.FunctionDecl name' (AST.FunctionSignature [] [returnType']) <$> genBlock body
  return [ AST.TopLevelComment (genSourceInfo' si)
         , function
         ]
genTopLevel name (Mono.TFn _ paramType returnType) (Fn si (NameBinding _ paramName) body _) = do
  name' <- genIdentifier name
  paramName' <- genIdentifier paramName
  paramType' <- genType paramType
  returnType' <- genType returnType
  function <- AST.FunctionDecl name' (AST.FunctionSignature [AST.FunctionParameter paramName' paramType'] [returnType']) <$> genBlock body
  return [ AST.TopLevelComment (genSourceInfo' si)
         , function
         ]
genTopLevel name type' expr = do
  var <- AST.VarDeclInitializer <$> genIdentifier name
                                <*> genType type'
                                <*> genExpr expr
  return [ AST.TopLevelComment (genSourceInfo (getSourceInfo expr))
         , AST.Decl (AST.VarDecl var)
         ]


genInstance :: InstantiatedDefinition -> Codegen [AST.TopLevelDeclaration]
genInstance = \case
  InstantiatedDefinition _fqn _si name expr ->
    genTopLevel name (typeOf expr) expr
  InstantiatedMethod _si name expr ->
    genTopLevel name (typeOf expr) expr


genMonomorphed :: MonomorphedDefinition -> Codegen [AST.TopLevelDeclaration]
genMonomorphed (MonomorphedDefinition _ name mt expr) =
  genTopLevel name mt expr


genImport :: ForeignPackageImport -> Codegen AST.ImportDecl
genImport (ForeignPackageImport _ name pkgPath) = do
  id' <- genIdentifier name
  return (AST.ImportDecl id' (AST.InterpretedStringLiteral pkgPath))

-- | Return the import alias name for the fmt package and possibly the code for
-- importing fmt (if not imported by the user).
getFmtImport :: [ForeignPackageImport] -> (GI.Identifier, Maybe AST.ImportDecl)
getFmtImport pkgs =
  case find isFmtPackage pkgs of
    Just (ForeignPackageImport _ (Identifier alias) _) ->
      (GI.Identifier alias, Nothing)
    Nothing ->
      let fmt = GI.Identifier "fmt"
      in (fmt, Just (AST.ImportDecl fmt (AST.InterpretedStringLiteral "fmt")))
  where
    isFmtPackage (ForeignPackageImport _ _ "fmt") = True
    isFmtPackage _ = False


prelude :: GI.Identifier -> [AST.TopLevelDeclaration]
prelude fmtAlias =
  let printSignature = AST.FunctionSignature [AST.FunctionParameter (GI.Identifier "x") (GT.Interface [])] []
      xOperand = AST.Expression (AST.Operand (AST.OperandName (GI.Identifier "x")))
      fmtApplication name = AST.Expression (AST.Application (AST.Operand (AST.QualifiedOperandName fmtAlias name)) (map AST.Argument [xOperand]))
  in [ AST.TopLevelComment (genSourceInfo Predefined)
     , AST.FunctionDecl
       (GI.Identifier "print")
       printSignature
       (AST.Block [AST.SimpleStmt (AST.ExpressionStmt (fmtApplication (GI.Identifier "Print")))])
     , AST.TopLevelComment (genSourceInfo Predefined)
     , AST.FunctionDecl
       (GI.Identifier "println")
       printSignature
       (AST.Block [AST.SimpleStmt (AST.ExpressionStmt (fmtApplication (GI.Identifier "Println")))])
  ]


genPackageName :: PackageDeclaration -> Codegen GI.Identifier
genPackageName (PackageDeclaration _ pkgName) =
  case pkgName of
    NativePackageName segments -> return (safeName (last segments))
    ForeignPackageName n -> return (safeName n)


genPackage :: MonomorphedPackage -> Codegen AST.SourceFile
genPackage (MonomorphedPackage pkgDecl imports is ms) = do
  pkgName <- genPackageName pkgDecl
  imports' <- mapM genImport imports

  let (fmtAlias, fmtImport) = getFmtImport imports
      allImports = imports' ++ maybeToList fmtImport

  is' <- concat <$> mapM genInstance (Set.toList is)
  ms' <- concat <$> mapM genMonomorphed (Set.toList ms)

  let allTopLevel = prelude fmtAlias ++ is' ++ ms'

  return (AST.SourceFile (AST.PackageClause pkgName) allImports allTopLevel)


toFilePath :: GoBackend -> PackageName -> Codegen FilePath
toFilePath (GoBackend goPath) =
  \case
    NativePackageName [] ->
      throwError (UnexpectedError "Package name cannot be empty")

    ForeignPackageName pkgName ->
      throwError
      (UnexpectedError $
        "Cannot codegen a package with a native package name: " ++ pkgName)

    NativePackageName segments ->
      let isMain = last segments == "main"
          dirParts = "src" : if isMain then init segments else segments
          dir = foldl (</>) goPath dirParts
          fileName = if isMain then "main.go" else last segments ++ ".go"
      in return (dir </> fileName)


instance Backend GoBackend where
  codegen backend pkg@(MonomorphedPackage (PackageDeclaration _ name) _ _ _) =
    runExcept (runReaderT action pkg)
    where action = do path <- toFilePath backend name
                      code <- genPackage pkg
                      return [CompiledFile path (displayS (renderPretty 0.4 120 (pretty code)) "")]

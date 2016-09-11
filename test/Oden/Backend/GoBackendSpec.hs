module Oden.Backend.GoBackendSpec where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Set              as Set hiding (map)

import           Oden.Backend
import           Oden.Backend.Go
import           Oden.Core.Expr
import           Oden.Core.Monomorphed
import           Oden.Core.Package

import qualified Oden.Go.AST           as AST
import qualified Oden.Go.Identifier    as GI
import qualified Oden.Go.Type          as GT

import           Oden.Identifier
import           Oden.Metadata
import           Oden.QualifiedName
import           Oden.SourceInfo
import           Oden.Type.Monomorphic

import           Test.Hspec

import           Oden.Assertions

missing = Metadata Missing

gen :: MonomorphedPackage -> Either CodegenError AST.SourceFile
gen pkg = runExcept (runReaderT (genPackage pkg) pkg)

identifierExpr s = AST.Expression (AST.Operand (AST.OperandName (GI.Identifier s)))

mainPkg = PackageDeclaration missing (NativePackageName ["main"])
fmtImport = AST.ImportDecl (GI.Identifier "fmt") (AST.InterpretedStringLiteral "fmt")

typeUnit = TCon missing (nameInUniverse "unit")
typeString = TCon missing (nameInUniverse "string")
typeChannel = TCon missing (nameInUniverse "Channel")

mainFn :: MonoTypedExpr -> MonomorphedDefinition
mainFn expr =
  MonomorphedDefinition
  missing
  (Identifier "main")
  (TNoArgFn missing typeUnit)
  (NoArgFn missing expr (TNoArgFn missing typeUnit))

literalExpr :: AST.Literal -> AST.Expression
literalExpr = AST.Expression . AST.Operand . AST.Literal

emptyStructLiteral :: AST.Expression
emptyStructLiteral =
  literalExpr
  (AST.CompositeLiteral
    (GT.Struct [])
    (AST.LiteralValueElements []))

spec :: Spec
spec =
  describe "genPackage" $ do
    it "includes the prelude" $
      gen (MonomorphedPackage mainPkg [] Set.empty Set.empty)
      `shouldSucceedWith'`
      AST.SourceFile
      (AST.PackageClause (GI.Identifier "main"))
      [fmtImport]
      (prelude (GI.Identifier "fmt"))

    it "translates an empty block for the main function" $
      gen (MonomorphedPackage
           mainPkg
           []
           Set.empty
           (Set.singleton (mainFn (Block missing [] typeUnit))))
      `shouldSucceedWith'`
      AST.SourceFile
      (AST.PackageClause (GI.Identifier "main"))
      [fmtImport]
      (prelude (GI.Identifier "fmt")
       ++ [ AST.TopLevelComment (AST.CompilerDirective "line <missing>:0")
          , AST.FunctionDecl
            (GI.Identifier "main")
            (AST.FunctionSignature [] [])
            (AST.Block [])
       ])

    it "returns an empty struct for an empty block in non-main functions" $
      gen (MonomorphedPackage
           mainPkg
           []
           Set.empty
           (Set.singleton (MonomorphedDefinition
                           missing
                           (Identifier "foo")
                           (TNoArgFn missing typeUnit)
                           (NoArgFn
                            missing
                            (Block missing [] typeUnit)
                            (TNoArgFn missing typeUnit)))))
      `shouldSucceedWith'`
      AST.SourceFile
      (AST.PackageClause (GI.Identifier "main"))
      [fmtImport]
      (prelude (GI.Identifier "fmt")
       ++ [ AST.TopLevelComment (AST.CompilerDirective "line <missing>:0")
          , AST.FunctionDecl
            (GI.Identifier "foo")
            (AST.FunctionSignature [] [GT.Struct []])
            (AST.Block [ AST.StmtComment (AST.CompilerDirective "line <missing>:0")
                       , AST.ReturnStmt [emptyStructLiteral]])
       ])

    it "translates the go unary op as an expression over a channel" $
      gen (MonomorphedPackage
           mainPkg
           []
           Set.empty
           (Set.singleton (MonomorphedDefinition
                           missing
                           (Identifier "foo")
                           (TApp missing typeChannel typeString)

                           (Go
                            missing
                            (Literal missing (String "ok") typeString)
                            (TApp missing typeChannel typeString)))))
       `shouldSucceedWith'`
       AST.SourceFile
      (AST.PackageClause (GI.Identifier "main"))
      [fmtImport]
      (prelude (GI.Identifier "fmt")
       ++ [ AST.TopLevelComment (AST.CompilerDirective "line <missing>:0")
          , AST.Decl
            (AST.VarDecl
             (AST.VarDeclInitializer
              (GI.Identifier "foo")
              (GT.Channel
               GT.Bidirectional
               (GT.Basic (GI.Identifier "string") False))
              (AST.Expression
               (AST.Application
                (AST.Operand
                 (AST.Literal
                  (AST.FunctionLiteral
                   (AST.FunctionSignature
                    []
                    [ GT.Channel
                      GT.Receive
                      (GT.Basic (GI.Identifier "string") False)
                    ])
                   (AST.Block [ AST.DeclarationStmt
                                (AST.VarDecl
                                 (AST.VarDeclInitializer
                                  (GI.Identifier "_go_ret")
                                  (GT.Channel
                                   GT.Bidirectional
                                   (GT.Basic
                                    (GI.Identifier "string")
                                    False))
                                  (AST.Expression
                                   (AST.Application
                                    (AST.Operand
                                     (AST.OperandName (GI.Identifier "make")))
                                    [ AST.TypeArgument
                                      (GT.Channel
                                       GT.Bidirectional
                                       (GT.Basic (GI.Identifier "string") False))
                                    , AST.Argument
                                      (AST.Expression
                                       (AST.Operand
                                        (AST.Literal
                                         (AST.BasicLiteral
                                          (AST.IntLiteral 1)))))
                                    ]))))
                              , AST.GoStmt
                                (AST.Expression
                                 (AST.Application
                                  (AST.Operand
                                   (AST.Literal
                                    (AST.FunctionLiteral
                                     (AST.FunctionSignature [] [])
                                     (AST.Block
                                      [AST.SimpleStmt
                                       (AST.SendStmt
                                        (AST.Expression
                                         (AST.Operand
                                          (AST.OperandName (GI.Identifier "_go_ret"))))
                                        (AST.Expression
                                         (AST.Operand
                                          (AST.Literal
                                           (AST.BasicLiteral
                                            (AST.StringLiteral
                                             (AST.InterpretedStringLiteral "ok")))))))
                                      ]))))
                                  []))
                              , AST.ReturnStmt
                                [AST.Expression
                                 (AST.Operand
                                  (AST.OperandName (GI.Identifier "_go_ret")))]
                              ]))))
                []))))
          ])

    it "writes line compiler directives" $
      gen (MonomorphedPackage
           mainPkg
           []
           Set.empty
           (Set.singleton (MonomorphedDefinition
                           missing
                           (Identifier "foo")
                           (TNoArgFn missing typeUnit)
                           (NoArgFn
                            (Metadata $ SourceInfo $ Position "foo.oden" 10 15)
                            (Literal
                             (Metadata $ SourceInfo $ Position "foo.oden" 11 5)
                             Unit
                             typeUnit)
                            (TNoArgFn missing typeUnit)))))
      `shouldSucceedWith'`
      AST.SourceFile
      (AST.PackageClause (GI.Identifier "main"))
      [fmtImport]
      (prelude (GI.Identifier "fmt")
       ++ [ AST.TopLevelComment (AST.CompilerDirective "line foo.oden:10")
          , AST.FunctionDecl
            (GI.Identifier "foo")
            (AST.FunctionSignature [] [GT.Struct []])
            (AST.Block [ AST.StmtComment (AST.CompilerDirective "line foo.oden:11")
                       , AST.ReturnStmt [emptyStructLiteral]
                       ])
       ])

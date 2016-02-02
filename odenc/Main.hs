{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Oden.Backend
import           Oden.Backend.Go
import           Oden.Compiler
import           Oden.Compiler.Validation
import qualified Oden.Core                       as Core
import qualified Oden.Core.Untyped               as Untyped
import qualified Oden.Env                        as Env
import           Oden.Explode
import qualified Oden.Go                         as Go
import           Oden.Infer
import qualified Oden.Output                     as Output
import           Oden.Output.Backend             ()
import           Oden.Output.Compiler            ()
import           Oden.Output.Compiler.Validation ()
import           Oden.Output.Explode             ()
import           Oden.Output.Go                  ()
import           Oden.Output.Infer               ()
import           Oden.Output.Instantiate         ()
import           Oden.Output.Parser              ()
import           Oden.Parser
import           Oden.Predefined
import           Oden.Scanner
import qualified Oden.Scope                      as Scope
import qualified Oden.Syntax                     as Syntax

import           Data.List
import           Data.Maybe
import qualified Data.Text.Lazy.IO               as L

import           Control.Monad.Except
import           Control.Monad.Reader

import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

-- versioning stuff
import qualified Data.Version                    as Version
import           Paths_oden

type Odenc = ReaderT Options (ExceptT String IO)

writeCompiledFile :: CompiledFile -> Odenc ()
writeCompiledFile (CompiledFile name contents) =
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory name)
    writeFile name contents

printOutput :: Output.OdenOutput o => o -> Odenc String
printOutput o = do
  opts <- ask
  let settings = Output.OutputSettings{ Output.monochrome = printMonochrome opts,
                                        Output.markdown = True }
  return (Output.print settings o)

printOutputs :: Output.OdenOutput o => [o] -> Odenc String
printOutputs = fmap unlines . mapM printOutput

liftEither :: Output.OdenOutput e => Either e b -> Odenc b
liftEither = either (printOutput >=> throwError) return

liftEither' :: Output.OdenOutput e => Either [e] b -> Odenc b
liftEither' = either (printOutputs >=> throwError) return

readPackage :: FilePath -> Odenc Syntax.Package
readPackage fname = do
  contents <- liftIO $ L.readFile fname
  liftEither $ parsePackage fname contents

logCompiling :: Core.Package -> Odenc ()
logCompiling (Core.Package name _ _) =
  liftIO (putStrLn $ "Compiling " ++ intercalate "/" name ++ "...")

logWarning :: Output.OdenOutput o => o -> Odenc ()
logWarning o = do
  s <- printOutput o
  liftIO (hPutStrLn stderr s)

scanImports :: Untyped.Package -> Odenc Scope.Scope
scanImports (Untyped.Package _ imports _) = foldM scanImport Scope.empty imports
  where scanImport scope' (Untyped.Import pn) = do
          (pkgScope, warning) <- liftIO (Go.getPackageScope pn) >>= liftEither
          case warning of
            Just w -> logWarning w
            _ -> return ()
          return (Scope.merge scope' pkgScope)

validatePkg :: Core.Package -> Odenc ()
validatePkg pkg = do
  warnings <- liftEither (validate pkg)
  mapM_ logWarning warnings

compileFile :: SourceFile -> Odenc [CompiledFile]
compileFile (OdenSourceFile fname _) = do
  opts <- ask
  -- TODO: Check package name
  syntaxPkg <- readPackage fname
  corePkg <- liftEither' (explodePackage syntaxPkg)
  importsScope <- scanImports corePkg
  let scope' = Scope.merge predefined importsScope
      typeEnv = Env.fromScope scope'
  (inferredPkg, _) <- liftEither (inferPackage typeEnv corePkg)
  validatePkg inferredPkg
  logCompiling inferredPkg
  compiledPkg <- liftEither (compile scope' inferredPkg)
  files <- liftEither (codegen (GoBackend $ outPath opts) compiledPkg)
  mapM_ writeCompiledFile files
  return files

odenc :: Odenc [CompiledFile]
odenc = do
  opts <- ask
  sourceFiles <- liftIO $ scan (odenPath opts </> "src")
  compiledFiles <- mapM compileFile sourceFiles
  return (concat compiledFiles)

exitWithMessage :: String -> IO ()
exitWithMessage err = do
  hPutStrLn stderr err
  exitFailure

logCompiledFiles :: [CompiledFile] -> IO ()
logCompiledFiles [_] = putStrLn "Compiled 1 Go source file."
logCompiledFiles files = putStrLn $ "Compiled " ++ show (length files) ++ " Go source files."

-- OPTIONS --

data Options = Options { showHelp        :: Bool
                       , showVersion     :: Bool
                       , odenPath        :: FilePath
                       , outPath         :: FilePath
                       , printMonochrome :: Bool
                       } deriving (Show, Eq, Ord)

defaultOptions :: Options
defaultOptions = Options { showHelp = False
                         , showVersion = False
                         , odenPath = "."
                         , outPath = "target/go"
                         , printMonochrome = False }

orMaybe :: Maybe a -> Maybe a -> Maybe a
(Just v) `orMaybe` _ = Just v
Nothing `orMaybe` m = m

setOdenPath :: Maybe FilePath -> Options -> IO Options
setOdenPath path opts = do
  envOdenPath <- lookupEnv "ODEN_PATH"
  return (opts { odenPath = fromMaybe (odenPath opts) (path `orMaybe` envOdenPath) })

setOutPath :: Maybe FilePath -> Options -> IO Options
setOutPath path opts = return (opts { outPath = fromMaybe (outPath opts) path })

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\opts -> return $ opts { showHelp = True }))
    "Print this help message"
  , Option ['V'] ["version"]
    (NoArg (\opts -> return $ opts { showVersion = True }))
    "Print the odenc version"
  , Option ['p'] ["oden-path"]
    (OptArg setOdenPath "DIR")
    "Search path for Oden sources"
  , Option ['o'] ["out-path"]
    (OptArg setOutPath "DIR")
    "GOPATH output directory"
  , Option ['M'] ["monochrome"]
    (NoArg (\opts -> return $ opts { printMonochrome = True }))
    "Print without colors"
  ]

usage :: String
usage = "Usage: odenc [OPTIONS]"

help :: String
help = usageInfo (usage ++ "\n\nOptions:\n") options

getOptions :: IO (Either String Options)
getOptions = do
  argv <- getArgs
  case getOpt Permute options argv of
    (o, _, []) -> Right <$> foldM (flip ($)) defaultOptions o
    (_, _, errs) -> return $ Left (concat errs ++ "\n" ++ help)

main :: IO ()
main = do
  o <- getOptions
  case o of
    Left err -> exitWithMessage err
    Right opts | showHelp opts -> putStrLn help
    Right opts | showVersion opts -> putStrLn (Version.showVersion version)
    Right opts -> do
      res <- runExceptT (runReaderT odenc opts)
      either exitWithMessage logCompiledFiles res

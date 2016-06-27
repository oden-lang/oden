import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.UserHooks
import Distribution.Simple.Setup

import System.Info
import System.Directory
import System.Environment
import System.Process
import System.FilePath

import Data.Maybe

main = defaultMainWithHooks simpleUserHooks { confHook = odenConfHook
                                            , postConf = odenPostConf }

addLibDirsToBuildInfo :: BuildInfo -> IO BuildInfo
addLibDirsToBuildInfo buildInfo = do
  wd <- getCurrentDirectory
  return $ buildInfo {
    extraLibDirs = wd : extraLibDirs buildInfo
  }

addLibDirsToOdenExe :: [Executable] -> IO [Executable]
addLibDirsToOdenExe = mapM addIfOden
  where
  addIfOden exe
    | exeName exe == "oden-exe" = do
      withLibDirs <- addLibDirsToBuildInfo (buildInfo exe)
      return $ exe { buildInfo = withLibDirs }
    | otherwise = return exe

addLibDirsToTests :: [TestSuite] -> IO [TestSuite]
addLibDirsToTests = mapM addLibDirs
  where
  addLibDirs suite = do
    withLibDirs <- addLibDirsToBuildInfo (testBuildInfo suite)
    return $ suite { testBuildInfo = withLibDirs }

odenConfHook :: (GenericPackageDescription, HookedBuildInfo)
             -> ConfigFlags
             -> IO LocalBuildInfo
odenConfHook (description, buildInfo) flags = do
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  let packageDescription = localPkgDescr localBuildInfo
      lib = fromJust $ library packageDescription

  libWithLibDirs <- addLibDirsToBuildInfo (libBuildInfo lib)
  executablesWithLibDirs <- addLibDirsToOdenExe (executables packageDescription)
  testSuitesWithLibDirs <- addLibDirsToTests (testSuites packageDescription)

  return $ localBuildInfo {
    localPkgDescr = packageDescription {
      library = Just $ lib {
        libBuildInfo = libWithLibDirs
      },
      executables = executablesWithLibDirs,
      testSuites = testSuitesWithLibDirs
    }
  }

ext :: String
ext = case os of
  "darwin"  -> ".dylib"
  "windows" -> ".dll"
  _         -> ".so"

odenPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
odenPostConf _ _ _ _ = do
  wd <- getCurrentDirectory
  setEnv "GOPATH" (wd </> "go")

  let dynamicPath = wd </> ("libimporter" ++ ext)
      buildDynamic = shell ("go build -buildmode=c-shared -o " ++ dynamicPath ++ " oden/cmd/importer")

  putStrLn $ "Compiling Go dynamic library to " ++ dynamicPath
  readCreateProcess buildDynamic "" >>= putStr

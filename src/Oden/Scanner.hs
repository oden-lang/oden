module Oden.Scanner (
  scan
) where

import           Control.Monad

import           System.Directory
import           System.FilePath
import           System.Posix.Files

import           Oden.QualifiedName
import           Oden.Path
import           Oden.SourceFile


isOdenSourceFile :: FilePath -> Bool
isOdenSourceFile = (== ".oden") . takeExtension


isRegularFileOrDirectory :: FilePath -> Bool
isRegularFileOrDirectory f = f /= "." && f /= ".."


scanAt :: [String] -> FilePath -> IO [SourceFile]
scanAt pkgSegments path = do
  s <- getFileStatus path
  if isDirectory s then do
    ds <- filter isRegularFileOrDirectory <$> getDirectoryContents path
    concat <$> forM ds scanDir
  else if isOdenSourceFile path
       then return [OdenSourceFile path (NativePackageName pkgSegments)]
       else return []
  where
  scanDir d =
    scanAt
    (pkgSegments ++ [dropExtension d])
    (path </> d)


scan :: OdenPath -> IO [SourceFile]
scan (OdenPath []) = error "empty Oden Path"
scan (OdenPath (root : _)) = scanAt [] (root </> "src")

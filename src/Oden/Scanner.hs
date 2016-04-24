module Oden.Scanner (
  SourceFile(..),
  scan
) where

import           Control.Monad
import           System.Directory
import           System.FilePath
import           System.Posix.Files

import           Oden.Core.Package

data SourceFile = OdenSourceFile FilePath PackageName
                deriving (Show, Eq, Ord)

isOdenSourceFile :: FilePath -> Bool
isOdenSourceFile = (== ".oden") . takeExtension

isRegularFileOrDirectory :: FilePath -> Bool
isRegularFileOrDirectory f = f /= "." && f /= ".."

scanAt :: PackageName -> FilePath -> IO [SourceFile]
scanAt pkgName path = do
  s <- getFileStatus path
  if isDirectory s then do
    ds <- filter isRegularFileOrDirectory <$> getDirectoryContents path
    concat <$> forM ds scanDir
  else if isOdenSourceFile path
       then return [OdenSourceFile path pkgName]
       else return []
  where
  scanDir d = do
    let path' = path </> d
        pkgName' = pkgName ++ [dropExtension d]
    scanAt pkgName' path'

scan :: FilePath -> IO [SourceFile]
scan = scanAt []

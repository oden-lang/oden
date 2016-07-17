module Oden.Scanner (
  SourceFile(..),
  scan
) where

import           Control.Monad

import           System.Directory
import           System.FilePath
import           System.Posix.Files

import           Oden.QualifiedName


data SourceFile = OdenSourceFile FilePath PackageName
                deriving (Show, Eq, Ord)


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


scan :: FilePath -> IO [SourceFile]
scan = scanAt []

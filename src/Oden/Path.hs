module Oden.Path
  ( OdenPath(..)
  , parseOdenPath
  , findPackageFileInPath
  )
where

import System.FilePath
import System.Directory


newtype OdenPath = OdenPath [FilePath] deriving (Show, Eq, Ord)


parseOdenPath :: String -> OdenPath
parseOdenPath = OdenPath . splitSearchPath


findFirstExistingFile :: [FilePath]
                      -> IO (Maybe FilePath)
findFirstExistingFile [] = return Nothing
findFirstExistingFile (p:ps) = do
  exists <- doesFileExist p
  if exists
    then return (Just p)
    else findFirstExistingFile ps

findPackageFileInPath :: [String]
           -> OdenPath
           -> IO (Maybe FilePath)
findPackageFileInPath segments (OdenPath roots) =
  findFirstExistingFile filePaths
  where
    relativePath = foldl (</>) "" segments ++ ".oden"
    filePathForRoot root = root </> "src" </> relativePath
    filePaths = map filePathForRoot roots

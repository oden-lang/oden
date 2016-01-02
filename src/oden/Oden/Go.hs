{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Oden.Go (
  PackageImportError(..),
  UnsupportedTypesWarning(..),
  getPackageScope
) where


import qualified Oden.Core                  as Core
import           Oden.Identifier
import           Oden.Go.Types              as G
import           Oden.Scope                 as Scope
import qualified Oden.Type.Polymorphic      as Poly

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (intercalate)
import qualified Data.Text                  as T
import qualified Data.Vector                as V

import           Foreign.C.String
foreign import ccall "GetPackageObjects" c_GetPackageObjects :: CString -> IO CString

optOrNull :: FromJSON a => HM.HashMap T.Text Value -> T.Text -> Parser (Maybe a)
o `optOrNull` k = case HM.lookup k o of
                    Just Null -> return Nothing
                    Just v -> parseJSON v
                    Nothing -> return Nothing

instance FromJSON Type where
  parseJSON (Object o) = do
    (kind :: String) <- o .: "kind"
    case kind of
      "basic"         -> Basic <$> o .: "name"
      "pointer"       -> Pointer <$> o .: "inner"
      "array"         -> G.Array <$> o .: "length"
                                 <*> o .: "inner"
      "slice"         -> Slice <$> o .: "inner"
      "signature"     -> Signature <$> o `optOrNull` "recv"
                                   <*> o .: "arguments"
                                   <*> o .: "returns"
      "named"         -> Basic <$> o .: "name"
      "unsupported"   -> Unsupported <$> o .: "name"
      k -> fail ("Unknown kind: " ++ k)
  parseJSON v = fail ("Unexpected: " ++ show v)

data PackageObject = PackageObject String Type
                   deriving (Show, Eq)

instance FromJSON PackageObject where
  parseJSON (Object o) = PackageObject <$> o .: "name"
                                       <*> o .: "type"

data PackageImportError = PackageImportError Core.PackageName String deriving (Show, Eq)

data PackageObjectsResponse = ErrorResponse String | ObjectsResponse [PackageObject]
                            deriving (Show, Eq)

instance FromJSON PackageObjectsResponse where
  parseJSON (Object o) = ErrorResponse <$> o .: "error"
                       <|> ObjectsResponse <$> o .: "objects"

decodeResponse :: Core.PackageName -> String -> Either PackageImportError [PackageObject]
decodeResponse pkgName s = either (Left . PackageImportError pkgName) Right $ do
  value <- eitherDecode (pack s)
  case value of
    ErrorResponse err -> Left err
    ObjectsResponse objs -> Right objs

data UnsupportedTypesWarning = UnsupportedTypesWarning { pkg :: Core.PackageName
                                                       , messages :: [(Name, String)]
                                                       } deriving (Show, Eq)

convertType :: G.Type -> Either String Poly.Type
convertType (Basic n) = return (Poly.TCon n)
convertType (Pointer n) = Left "Pointers"
convertType (G.Array l t) = Left "Arrays"
convertType (Slice t) = Left "Slices"
convertType (Signature (Just _) _ _) = Left "Methods (functions with receivers)"
convertType (Signature Nothing [] [return]) =
  Poly.TArrSingle <$> convertType return
convertType (Signature Nothing [arg] [return]) = do
  a <- convertType arg
  r <- convertType return
  Right (Poly.TArr a r)
convertType (Signature Nothing a _) | length a > 1 = Left "Functions with multiple arguments"
convertType (Signature Nothing _ r) | length r > 1 = Left "Functions with multiple return values"
-- TODO: Add "Named" concept in Oden type system
convertType (Named pkg n t) = convertType t
convertType (Unsupported n) = Left n

objectsToScope :: Core.PackageName -> [PackageObject] -> (Scope, Maybe UnsupportedTypesWarning)
objectsToScope pkgName objs =
  case foldl addObject (Scope.empty, []) objs of
    (s, []) -> (s, Nothing)
    (s, msgs) -> (s, Just (UnsupportedTypesWarning pkgName msgs))
  where
  addObject (scope, us) (PackageObject n t) =
    case convertType t of
      Left u -> (scope, (n, u) : us)
      Right ct ->
        let i = Qualified (last pkgName) n
            sc = Poly.Forall [] ct
        in (Scope.insert (Scope.Import pkgName) i (ForeignDefinition i sc) scope,
            us)

getPackageScope :: Core.PackageName -> IO (Either PackageImportError (Scope, Maybe UnsupportedTypesWarning))
getPackageScope pkgName = do
  cs <- newCString (intercalate "/" pkgName)
  objs <- decodeResponse pkgName <$> (c_GetPackageObjects cs >>= peekCString)
  return (objectsToScope pkgName <$> objs)

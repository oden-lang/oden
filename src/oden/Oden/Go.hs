{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Oden.Go (
  PackageImportError(..),
  UnsupportedTypesWarning(..),
  getPackageScope
) where


import qualified Oden.Core                  as Core
import           Oden.Go.Types              as G
import           Oden.Identifier
import           Oden.Scope                 as Scope
import qualified Oden.Type.Polymorphic      as Poly

import           Control.Applicative        hiding (Const)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (intercalate)
import qualified Data.Text                  as T

import           Foreign.C.String
foreign import ccall "GetPackageObjects" c_GetPackageObjects :: CString -> IO CString

optOrNull :: FromJSON a => HM.HashMap T.Text Value -> T.Text -> Parser (Maybe a)
o `optOrNull` k = case HM.lookup k o of
                    Just Null -> return Nothing
                    Just v -> parseJSON v
                    Nothing -> return Nothing

instance FromJSON Type where
  parseJSON (Object o) = do
    kind :: String <- o .: "kind"
    case kind of
      "basic"         -> Basic <$> o .: "name" <*> o .: "untyped"
      "pointer"       -> Pointer <$> o .: "inner"
      "array"         -> G.Array <$> o .: "length"
                                 <*> o .: "inner"
      "slice"         -> Slice <$> o .: "inner"
      "signature"     -> Signature <$> o `optOrNull` "recv"
                                   <*> o .: "arguments"
                                   <*> o .: "returns"
      "named"         -> Named <$> o .: "pkg" <*> o .: "name" <*> o .: "underlying"
      "unsupported"   -> Unsupported <$> o .: "name"
      k -> fail ("Unknown kind: " ++ k)
  parseJSON v = fail ("Unexpected: " ++ show v)

data PackageObject = Func Name Type
                   | Var Name Type
                   | Const Name Type
                   deriving (Show, Eq)

nameOf :: PackageObject -> Name
nameOf (Func n _) = n
nameOf (Var n _) = n
nameOf (Const n _) = n

typeOf :: PackageObject -> Type
typeOf (Func _ t) = t
typeOf (Var _ t) = t
typeOf (Const _ t) = t

instance FromJSON PackageObject where
  parseJSON (Object o) = do
    t :: String <- o .: "objectType"
    case t of
      "func"  -> Func <$> o .: "name" <*> o .: "type"
      "var"   -> Var <$> o .: "name" <*> o .: "type"
      "const" -> Const <$> o .: "name" <*> o .: "type"

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

data UnsupportedTypesWarning = UnsupportedTypesWarning { pkg      :: Core.PackageName
                                                       , messages :: [(Name, String)]
                                                       } deriving (Show, Eq)

convertType :: G.Type -> Either String Poly.Type
convertType (Basic n False) = return (Poly.TCon n)
-- TODO: Add "Untyped constant" concept in Oden type system
-- and/or consider how macros would relate to this.
convertType (Basic "bool" True) = return Poly.typeBool
convertType (Basic "int" True) = return Poly.typeInt
convertType (Basic "rune" True) = return Poly.typeInt
convertType (Basic "float" True) = return (Poly.TCon "float32")
convertType (Basic "complex" True) = return (Poly.TCon "complex64")
convertType (Basic "string" True) = return (Poly.TCon "string")
convertType (Basic "nil" True) = Left "nil constants"
convertType (Basic n True) = Left ("Untyped " ++ n ++ "s")
convertType (Pointer _) = Left "Pointers"
convertType (G.Array _ _) = Left "Arrays"
convertType (Slice t) = Poly.TSlice <$> convertType t
convertType (Signature (Just _) _ _) = Left "Methods (functions with receivers)"
convertType (Signature Nothing args []) = do
  as <- mapM convertType args
  Right (Poly.TGoFunc as Poly.typeUnit)
convertType (Signature Nothing args [ret]) = do
  as <- mapM convertType args
  r <- convertType ret
  Right (Poly.TGoFunc as r)
convertType (Signature Nothing _ _) = Left "Functions with multiple return values"
-- TODO: Add "Named" concept in Oden type system
convertType (Named _ _ t) = convertType t
convertType (Unsupported n) = Left n

objectsToScope :: Core.PackageName -> [PackageObject] -> (Scope, Maybe UnsupportedTypesWarning)
objectsToScope pkgName objs =
  case foldl addObject (Scope.empty, []) objs of
    (s, []) -> (s, Nothing)
    (s, msgs) -> (s, Just (UnsupportedTypesWarning pkgName msgs))
  where
  addObject (scope, us) o =
    let n = nameOf o
        t = typeOf o
    in case convertType t of
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

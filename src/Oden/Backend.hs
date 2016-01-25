module Oden.Backend where

import Oden.Compiler

data CodegenError = UnexpectedError String
                  deriving (Show, Eq, Ord)

data CompiledFile = CompiledFile FilePath String
                  deriving (Show, Eq, Ord)

class Backend b where
  codegen :: b -> CompiledPackage -> Either CodegenError [CompiledFile]

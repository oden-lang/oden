module Oden.Identifier (
  Identifier(..),
  IdentifierValidationError,
  errorMessage,
  createLegalIdentifier,
  asString
) where

import Data.Char

newtype Identifier = Identifier String deriving (Show, Eq, Ord)

data IdentifierValidationError = Empty
                               | IllegalStart Char
                               | IllegalCharacters [Char]

errorMessage :: IdentifierValidationError -> String
errorMessage Empty =
  "Identifier cannot be empty"
errorMessage (IllegalStart c) =
  "Identifier cannot start with '" ++ [c] ++ "'"
errorMessage (IllegalCharacters chars) =
  "Identifier cannot contain any of '" ++ chars ++ "'"

createLegalIdentifier :: String -> Either IdentifierValidationError Identifier
createLegalIdentifier [] = Left Empty
createLegalIdentifier (first:_) | not (isLetter first) = Left (IllegalStart first)
createLegalIdentifier s = case filter (not . legal) s of
                  []    -> Right (Identifier s)
                  chars -> Left (IllegalCharacters chars)
  where
  legal c = isLetter c
            || isNumber c
            || c `elem` "_-'!?"

asString :: Identifier -> String
asString (Identifier s) = s

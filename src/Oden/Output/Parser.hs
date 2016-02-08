module Oden.Output.Parser where

import qualified Data.Set as Set

import Text.PrettyPrint
import Text.Parsec hiding (unexpected)
import Text.Parsec.Error

import Oden.Output as Output
import Oden.SourceInfo

type ExpectedParts = Set.Set String
type UnexpectedPart = Maybe String
data CondensedMessage = CondensedMessage ExpectedParts UnexpectedPart

condense :: ParseError -> CondensedMessage
condense e = foldl iter (CondensedMessage Set.empty Nothing) (errorMessages e)
  where
  iter (CondensedMessage ex _) (SysUnExpect s) =
    CondensedMessage ex (Just s)
  iter (CondensedMessage ex _) (UnExpect s) =
    CondensedMessage ex (Just s)
  iter c@(CondensedMessage ex u) (Expect s)
    | s == "" = c
    | otherwise = CondensedMessage (Set.insert s ex) u
  iter c _ = c


commaOr :: [String] -> Doc
commaOr [] = empty
commaOr [x] = text x
commaOr xs = hcat (punctuate (text ", ") (map text (init xs)))
             <+> text "or" <+> text (last xs)

errorSourceInfo :: ParseError -> SourceInfo
errorSourceInfo e =
   let pos = errorPos e
   in SourceInfo (Position (sourceName pos)
                           (sourceLine pos)
                           (sourceColumn pos))

instance OdenOutput ParseError where
  outputType _ = Output.Error
  name _ = "Parser.ParseError"
  header e _ =
    case condense e of
      (CondensedMessage ex Nothing) ->
        text "Expected" <+> hcat (punctuate (text ", ") (map text (Set.toList ex)))
      (CondensedMessage ex (Just unEx)) ->
        text "Expected" <+> commaOr (Set.toList ex)
        <+> text "but got:" <+> text unEx
  details _ _ = empty
  sourceInfo e = Just (errorSourceInfo e)


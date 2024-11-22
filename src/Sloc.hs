module Sloc where

import Data.Char (isSpace)

data Token
  = Comment
  | CodeLine String
  | Empty
  deriving (Show, Eq)

data ParserState
  = InMultiLineComment
  | InSingleLineComment
  | InStringLiteral
  | Normal
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize = undefined

tokenize' :: [String] -> String -> ParserState -> [Token]
tokenize' [] _ _ = [Empty]
tokenize' (l : lss) acc state = processline l ++ tokenize' lss acc state

processline :: String -> String -> ParserState -> [Token]
processline [] _ InMultiLineComment = [Comment]
processline [] _ InStringLiteral = [CodeLine []]
processline [] _ Normal = [Empty]
processline [] _ InSingleLineComment = error "Unexpected Behaviour, parsing empty line while reading InLineComment"
processline (c : cs) acc InMultiLineComment =
  if (not . null) cs
    then if c == '*' && head cs == '/' then Comment : processline (tail cs) acc Normal else processline (tail cs) acc InMultiLineComment
    else [Comment]
processline line acc InSingleLineComment = [Comment]

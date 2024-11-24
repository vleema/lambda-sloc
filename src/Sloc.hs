module Sloc where

import Data.Bifunctor (Bifunctor (second))

data Token
  = CommentLine
  | CodeLine
  | BlankLine
  deriving (Show, Eq)

data ParserState
  = InMultiLineComment
  | InStringLiteral
  | Normal
  deriving (Show, Eq)

countToken :: Token -> [Token] -> Int
countToken t = length . filter (== t)

tokenize :: String -> [Token]
tokenize fileContent = processlines (filter (not . null) $ lines fileContent) Normal ++ blankLines
 where
  blankLines = [BlankLine | l <- lines fileContent, null l]

processlines :: [String] -> ParserState -> [Token]
processlines [] _ = []
processlines (l : lss) state =
  let (lastState, tokens) = processline (trim l) [] state
   in tokens ++ processlines lss lastState

processline :: String -> String -> ParserState -> (ParserState, [Token])
processline [] acc Normal = (Normal, [CodeLine | not $ null acc])
processline [] _ InStringLiteral = (InStringLiteral, [CodeLine])
processline [] _ InMultiLineComment = (InMultiLineComment, [CommentLine])
processline (c : cs) _ InMultiLineComment
  | not (null cs) && c == '*' && head cs == '/' = (Normal, CommentLine : snd (processline (tail cs) [] Normal))
  | otherwise = processline cs [] InMultiLineComment
processline (c : cs) acc InStringLiteral
  | isStringLiteralEnd c acc = processline cs (c : acc) Normal
  | otherwise = processline cs (c : acc) InStringLiteral
processline (c : cs) acc Normal
  | isStringLiteralStart c acc && isNotSingleQuoteBefore acc = processline cs (c : acc) InStringLiteral
  | not (null cs) && c == '/' && head cs == '/' = (Normal, [CodeLine | not $ null acc] ++ [CommentLine])
  | not (null cs) && c == '/' && head cs == '*' = second (CodeLine :) $ processline (tail cs) [] InMultiLineComment
  | otherwise = processline cs (c : acc) Normal

isStringLiteralStart :: Char -> String -> Bool
isStringLiteralStart c acc = (c == '"' || c == '`') && (null acc || not (isEscaped acc))

isNotSingleQuoteBefore :: String -> Bool
isNotSingleQuoteBefore acc = null acc || head acc /= '\''

isEscaped :: String -> Bool
isEscaped = odd . length . takeWhile (== '\\')

isStringLiteralEnd :: Char -> String -> Bool
isStringLiteralEnd c acc = (c == '"' || c == '`') && not (isEscaped acc)

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile (`elem` " \t\n\v\f\r")

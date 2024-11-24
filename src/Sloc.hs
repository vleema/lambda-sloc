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
tokenize fileContent = tokenize' (filter (not . null) $ lines fileContent) Normal ++ blankLines
 where
  blankLines = [BlankLine | l <- lines fileContent, null l]

tokenize' :: [String] -> ParserState -> [Token]
tokenize' [] _ = []
tokenize' (l : lss) state =
  let (lastState, tokens) = processline (trim l) [] state
   in tokens ++ tokenize' lss lastState

processline :: String -> String -> ParserState -> (ParserState, [Token])
processline [] acc Normal = (Normal, [CodeLine | not $ null acc])
processline [] _ InStringLiteral = (Normal, [CodeLine])
processline [] _ InMultiLineComment = (InMultiLineComment, [CommentLine])
processline (c : cs) _ InMultiLineComment
  | not (null cs) && c == '*' && head cs == '/' = (Normal, CommentLine : snd (processline (tail cs) [] Normal))
  | otherwise = processline cs [] InMultiLineComment
processline (c : cs) acc InStringLiteral
  | isStringLiteralEnd c acc || isNotEscapedSingleQuote acc = processline cs (c : acc) Normal
  | otherwise = processline cs (c : acc) InStringLiteral
processline (c : cs) acc Normal
  | isStringLiteralStart c acc = processline cs (c : acc) InStringLiteral
  | not (null cs) && c == '/' && head cs == '/' = (Normal, [CodeLine | not $ null acc] ++ [CommentLine])
  | not (null cs) && c == '/' && head cs == '*' = second (CodeLine :) $ processline (tail cs) [] InMultiLineComment
  | otherwise = processline cs (c : acc) Normal

isStringLiteralStart :: Char -> String -> Bool
isStringLiteralStart c acc = c == '"' && (null acc || last acc /= '\\')

isNotEscapedSingleQuote :: String -> Bool
isNotEscapedSingleQuote acc = (not . null) acc && head acc /= '\''

countBackSlashes :: String -> Int
countBackSlashes = length . takeWhile (== '\\')

isStringLiteralEnd :: Char -> String -> Bool
isStringLiteralEnd ch predChars = ch == '"' && even (countBackSlashes predChars)

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile (`elem` " \t\n\v\f\r")

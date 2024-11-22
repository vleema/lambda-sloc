module Sloc where

import Data.Char (isSpace)

data Token
  = Comment
  | CodeLine
  | Empty
  deriving (Show, Eq)

data ParserState
  = InMultiLineComment
  | InStringLiteral
  | Normal
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize fileContent = tokenize' (lines fileContent) Normal

tokenize' :: [String] -> ParserState -> [Token]
tokenize' [] _ = []
tokenize' (l : lss) state = processline l [] state ++ tokenize' lss state

processline :: String -> String -> ParserState -> [Token]
processline [] acc Normal = if null acc then [Empty] else [CodeLine]
processline [] _ InStringLiteral = [CodeLine]
processline [] _ InMultiLineComment = [Comment]
processline (c : cs) acc InMultiLineComment
  | not (null cs) && c == '*' && head cs == '/' = Comment : processline (tail cs) acc Normal
  | otherwise = processline cs acc InMultiLineComment
processline (c : cs) acc InStringLiteral
  | isStringLiteralEnd c acc || isNotEscapedSingleQuote acc = processline cs (c : acc) Normal
  | otherwise = processline cs (c : acc) InStringLiteral
processline (c : cs) acc Normal
  | all isSpace (c : cs) = [Empty]
  | isStringLiteralStart c acc = processline cs (c : acc) InStringLiteral
  | not (null cs) && c == '/' && head cs == '/' = [CodeLine, Comment]
  | not (null cs) && c == '/' && head cs == '*' = CodeLine : processline (tail cs) [] InMultiLineComment
  | otherwise = processline cs (c : acc) Normal

isStringLiteralStart :: Char -> String -> Bool
isStringLiteralStart c acc = c == '"' && (null acc || last acc /= '\\')

isNotEscapedSingleQuote :: String -> Bool
isNotEscapedSingleQuote acc = (not . null) acc && head acc /= '\''

countBackSlashes :: String -> Int
countBackSlashes = length . takeWhile (== '\\')

isStringLiteralEnd :: Char -> String -> Bool
isStringLiteralEnd ch predChars = ch == '"' && even (countBackSlashes predChars)

testTokenize :: IO ()
testTokenize = do
  fileContents <- readFile "test-files/test.cpp"
  print (tokenize fileContents)

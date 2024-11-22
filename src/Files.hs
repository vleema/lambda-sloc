module Files where

import Control.Monad (filterM, forM)
import GHC.OldList (isSuffixOf)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

import Sloc (Token (BlankLine, CodeLine), countToken, tokenize)

data FileType
  = Java FilePath
  | C FilePath
  | CPP FilePath
  | Rust FilePath
  | Header FileType
  | None
  deriving (Show, Eq)

data File = File
  { loc :: Int
  , total :: Int
  , blank :: Int
  , file :: FileType
  }

processFiles :: [FilePath] -> [IO File]
processFiles = map processFile

processFile :: FilePath -> IO File
processFile fs = do
  fileContents <- readFile fs
  let tokens = tokenize fileContents
  return File{loc = countToken CodeLine tokens, total = length fileContents, blank = countToken BlankLine tokens, file = sourceCodeFile fs}

sourceCodeFile :: FilePath -> FileType
sourceCodeFile file_
  | ".java" `isSuffixOf` file_ = Java file_
  | ".c" `isSuffixOf` file_ = C file_
  | ".cpp" `isSuffixOf` file_ = CPP file_
  | ".h" `isSuffixOf` file_ = Header (C file_)
  | ".hpp" `isSuffixOf` file_ = Header (CPP file_)
  | ".rs" `isSuffixOf` file_ = Rust file_
  | otherwise = None

listFilesRecursiverly :: FilePath -> IO [FilePath]
listFilesRecursiverly filePath = do
  dirContents <- listDirectory filePath
  let fullPaths = map (filePath </>) dirContents
  commonFiles <- filterM doesFileExist fullPaths
  directories <- filterM doesDirectoryExist fullPaths
  nestedFiles <- forM directories listFilesRecursiverly
  return $ commonFiles ++ concat nestedFiles

getSourceCodeFiles :: Bool -> FilePath -> IO [FileType]
getSourceCodeFiles recursively filePath = do
  isDir <- doesDirectoryExist filePath
  if isDir
    then do
      commonFiles <- if recursively then listFilesRecursiverly filePath else listDirectory filePath
      return $ [sourceCodeFile file_ | file_ <- commonFiles, sourceCodeFile file_ /= None]
    else case sourceCodeFile filePath of
      None -> return []
      _ -> return [sourceCodeFile filePath]

testGetSourceCodeFiles :: FilePath -> IO ()
testGetSourceCodeFiles fs = do
  files <- getSourceCodeFiles True fs
  print files

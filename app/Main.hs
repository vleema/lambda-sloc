module Main where

import Control.Monad (filterM, forM, when)
import GHC.OldList (isSuffixOf)
import SlocCLIParser (files, help, helpMessage, parseCommandLineArgs, recursive)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

main :: IO ()
main = do
  cliArgs <- getArgs
  let runningOptions = parseCommandLineArgs cliArgs
  when (null (files runningOptions) && not (help runningOptions)) $ do
    putStrLn "Required argument: \'file/directory\' not set"
    putStrLn "Run with --help flag for more information"
    exitFailure
  when (help runningOptions) $ do
    putStrLn helpMessage
    exitSuccess
  sourceCodeFiles <- forM (files runningOptions) $ getSourceCodeFiles (recursive runningOptions)
  mapM_ putStrLn $ concat sourceCodeFiles

isSourceCodeFile :: FilePath -> Bool
isSourceCodeFile file = any (`isSuffixOf` file) [".java", ".cpp", ".rs"]

listFilesRecursiverly :: FilePath -> IO [FilePath]
listFilesRecursiverly filePath = do
  dirContents <- listDirectory filePath
  let fullPaths = map (filePath </>) dirContents
  commonFiles <- filterM doesFileExist fullPaths
  directories <- filterM doesDirectoryExist fullPaths
  nestedFiles <- forM directories listFilesRecursiverly
  return $ commonFiles ++ concat nestedFiles

getSourceCodeFiles :: Bool -> FilePath -> IO [FilePath]
getSourceCodeFiles recursively filePath = do
  isDir <- doesDirectoryExist filePath
  if isDir
    then do
      commonFiles <- if recursively then listFilesRecursiverly filePath else listDirectory filePath
      filterM (return . isSourceCodeFile) commonFiles
    else if isSourceCodeFile filePath then return [filePath] else return []

